{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Post where

import Database.HDBC (SqlValue, handleSql, run, commit, quickQuery', fromSql, toSql)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersect, union, intercalate)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Control.Exception as Exc

import Post.DB.DBSpec (Handle(..))
import qualified Post.Logger as Logger
import qualified Post.Exception as E
import qualified Post.DB.Author as DBA
import qualified Post.DB.Category as DBC
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Photo as DBPh
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Data as DbData
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Post
createPost :: Handle IO -> Title -> Text -> AuthorId -> CategoryId -> [TagId] -> IO (Maybe PostId)
createPost handle title text authorId catId tagIds = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r1 <- quickQuery' dbh "SELECT id \
                        \FROM posts \
                        \WHERE title = ?"
         [toSql title]
  case r1 of
    [] -> do
      time <- getCurrentTime
      --let createdAt = utctDay time
      _ <- run dbh "INSERT INTO posts (title, text, created_at) \
                   \VALUES (?,?,?)"
            [toSql title, toSql text, toSql time]
      commit dbh
      Logger.logInfo logh $ "Post with title: "
        <> title
        <> " was successfully inserted in db."
      r2 <- quickQuery' dbh "SELECT id \
                            \FROM posts \
                            \ORDER BY id DESC LIMIT 1" []
      case r2 of
        [[idPost]] -> do
          let postId = fromSql idPost
          createPostAuthorDep handle postId authorId
          createPostCatDep handle postId catId
          mapM_ (createPostTagDep handle postId) tagIds
          return $ Just postId
        _ -> do
          Logger.logError logh "Error while inserting Post to db."
          return Nothing
    _ -> do
      Logger.logWarning logh $ "Post with title: "
        <> title
        <> " already exists in db."
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in createPost!\n"
            <> show e

getPosts :: Handle IO -> DbData.DbReq -> IO (Maybe [Post])
getPosts handle dbReq = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  -- Request to DB table posts
      dbPostQuery = "SELECT id \
                    \FROM posts "
        <> fst (DbData.dbPostReq dbReq)
      dbPostArgs = snd $ DbData.dbPostReq dbReq
  Logger.logDebug logh $ "Query to table 'posts': "
    <> T.pack dbPostQuery
  rPost <- quickQuery' dbh dbPostQuery dbPostArgs
  -- Request to DB table post_category
  let dbCatQuery = "SELECT post_id \
                   \FROM post_category "
        <> fst (DbData.dbCatReq dbReq)
      dbCatArgs = snd $ DbData.dbCatReq dbReq
  Logger.logDebug logh $ "Query to table 'post_category': "
    <> T.pack dbCatQuery
  rCat <- quickQuery' dbh dbCatQuery dbCatArgs
  -- Request to DB table post_tag
  let dbTagQuery = "SELECT post_id \
                   \FROM post_tag "
        <> fst (DbData.dbTagReq dbReq)
      dbTagArgs = snd $ DbData.dbTagReq dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': "
    <> T.pack dbTagQuery
  rTag <- quickQuery' dbh dbTagQuery dbTagArgs
  -- Request to DB table post_author
  let dbAuthorQuery = "SELECT post_id \
                      \FROM post_author "
        <> fst (DbData.dbAuthorReq dbReq)
      dbAuthorArgs = snd $ DbData.dbAuthorReq dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': "
    <> T.pack dbAuthorQuery
  rAuthor <- quickQuery' dbh dbAuthorQuery dbAuthorArgs
  -- Serch request to DB table posts
  let dbPostSearchQuery = "SELECT id \
                          \FROM posts "
        <> fst (DbData.dbPostSearch dbReq)
      dbPostSearchArgs = snd $ DbData.dbPostSearch dbReq
  Logger.logDebug logh $ "Search Query to table 'posts': "
    <> T.pack dbPostSearchQuery
  rPostSearch <- quickQuery' dbh dbPostSearchQuery dbPostSearchArgs
  -- Serch request to DB table post_author
  let dbAuthorSearchQuery = "SELECT post_id \
                            \FROM post_author "
        <> fst (DbData.dbAuthorSearch dbReq)
      dbAuthorSearchArgs = snd $ DbData.dbAuthorSearch dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': "
    <> T.pack dbAuthorSearchQuery
  rAuthorSearch <- quickQuery' dbh dbAuthorSearchQuery dbAuthorSearchArgs
  -- Serch request to DB table post_category
  let dbCatSearchQuery = "SELECT post_id \
                         \FROM post_category "
        <> fst (DbData.dbCatSearch dbReq)
      dbCatSearchArgs = snd $ DbData.dbCatSearch dbReq
  Logger.logDebug logh $ "Query to table 'post_category': "
    <> T.pack dbCatSearchQuery
  rCatSearch <- quickQuery' dbh dbCatSearchQuery dbCatSearchArgs
  -- Serch request to DB table post_tag
  let dbTagSearchQuery = "SELECT post_id \
                         \FROM post_tag "
        <> fst (DbData.dbTagSearch dbReq)
      dbTagSearchArgs = snd $ DbData.dbTagSearch dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': "
    <> T.pack dbTagSearchQuery
  rTagSearch <- quickQuery' dbh dbTagSearchQuery dbTagSearchArgs
  -- Requests
  let rAllSimple = ((rPost `intersect` rCat) `intersect` rTag) `intersect` rAuthor
      rAllSearch = ((rPostSearch `union` rCatSearch) `union` rTagSearch) `union` rAuthorSearch
      rAll = rAllSimple `intersect` rAllSearch
  -- Request with sorting
  case rAll of
    [] -> do
      Logger.logWarning logh "No posts in db!"
      return Nothing
    _ -> do
      let idAll = concat rAll
          nAll = length idAll
      Logger.logInfo logh "Sorting posts from db!"
      rMain <- quickQuery' dbh (DbData.orderQuery dbReq
        <> intercalate "," (replicate nAll "?")
        <> DbData.orderBy dbReq) idAll
      case rMain of
        [] -> do
          Logger.logWarning logh "No posts in db!"
          return Nothing
        ids -> do
          posts <- mapM (getPost handle . fromSql) $ concat ids
          return $ sequence posts
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPosts!\n"
            <> show e

getPost :: Handle IO -> PostId -> IO (Maybe Post)
getPost handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title, created_at, text \
                       \FROM posts \
                       \WHERE id = ?"
        [toSql postId]
  case r of
    [params] -> do
      Logger.logInfo logh "Getting Post from db."
      newPost handle params
    _ -> do
      Logger.logWarning logh $ "No post with id: "
        <> convert postId
        <> " in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPost!\n"
            <> show e

removePost :: Handle IO -> PostId -> IO (Maybe PostId)
removePost handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id \
                       \FROM posts \
                       \WHERE user_id = ?"
       [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Post with id: "
        <> convert postId
        <> "!"
      return Nothing
    _ -> do
      _ <- run dbh "DELETE FROM posts \
                   \WHERE id = ?"
           [toSql postId]
      _ <- removePostAuthorDep handle postId
      _ <- removePostCatDep handle postId
      _ <- removePostTagDep handle postId
      _ <- removePostMainPhotoDep handle postId
      _ <- removePostAddPhotoDep handle postId
      _ <- removePostCommentDep handle postId
      _ <- removePostDraftDep handle postId
      commit dbh
      Logger.logInfo logh $ "Removing Post with id: "
        <> convert postId
        <> " from db."
      return $ Just postId
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePost!\n"
            <> show e

setPostMainPhoto :: Handle IO -> PostId -> Text -> IO (Maybe PhotoId)
setPostMainPhoto handle postId path = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  photoIdMaybe <- DBPh.savePhoto handle path
  case photoIdMaybe of
    Nothing -> do
      Logger.logError logh $ "Couldn't set Main Photo for Post with id: "
        <> convert postId
      return Nothing
    Just photoId -> do
      r <- quickQuery' dbh "SELECT photo_id \
                           \FROM post_main_photo \
                           \WHERE post_id = ?" 
            [toSql postId]
      case r of
        [] -> do
          _ <- run dbh "INSERT INTO post_main_photo (photo_id, post_id) \
                       \VALUES (?,?)" 
                [toSql photoId, toSql postId]
          commit dbh
          Logger.logInfo logh "Post's Main Photo was successfully set."
          return $ Just photoId
        _ -> do
          _ <- run dbh "UPDATE post_main_photo \
                       \SET photo_id = ? \
                       \WHERE post_id = ?" 
                [toSql photoId, toSql postId]
          commit dbh
          Logger.logInfo logh "Post's Main Photo was successfully updated."
          return $ Just photoId
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in setPostMainPhoto!\n"
            <> show e

setPostAddPhoto :: Handle IO -> PostId -> Text -> IO (Maybe PhotoId)
setPostAddPhoto handle postId path = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  photoIdMaybe <- DBPh.savePhoto handle path
  case photoIdMaybe of
    Nothing -> do
      Logger.logError logh $ "Couldn't set Additional Photo for Post with id: "
        <> convert postId
      return Nothing
    Just photoId -> do
      r <- quickQuery' dbh "SELECT photo_id \
                           \FROM post_add_photo \
                           \WHERE post_id = ? AND photo_id = ?" 
           [toSql postId, toSql photoId]
      case r of
        [] -> do
          _ <- run dbh "INSERT INTO post_add_photo (photo_id, post_id) \
                        \VALUES (?,?)" 
                [toSql photoId, toSql postId]
          commit dbh
          Logger.logInfo logh "Post's Add Photo was successfully set."
          return $ Just photoId
        _ -> do
          Logger.logError logh $ "Add Photo with id: "
            <> convert photoId
            <> " for Post with id: "
            <> convert postId
            <> " already exists in db."
          return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in setPostAddPhoto!\n"
            <> show e

getPostMainPhoto :: Handle IO -> PostId -> IO (Maybe Photo)
getPostMainPhoto handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT photo_id \
                       \FROM post_main_photo \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [[photoId]] -> do
      Logger.logInfo logh $ "Getting Main Photo for Post with id: "
        <> convert postId <> "."
      DBPh.getPhoto handle $ fromSql photoId
    _ -> do
      Logger.logWarning logh $ "No exists Main Photo for Post with id: "
        <> convert postId
        <> " in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPostMainPhoto!\n"
            <> show e

getPostAddPhotos :: Handle IO -> PostId -> IO (Maybe [Photo])
getPostAddPhotos handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT photo_id \
                       \FROM post_add_photo \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Add Photos for Post with id: "
        <> convert postId
        <> " in db!"
      return Nothing
    ids -> do
      Logger.logInfo logh $ "Getting Add Photos for Post with id: "
        <> convert postId
        <> "."
      let addPhotoIds = map fromSql $ concat ids
      addPhotos <- mapM (DBPh.getPhoto handle) addPhotoIds
      return $ sequence addPhotos
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPostAddPhotos!\n"
            <> show e

getPostComments :: Handle IO -> PostId -> IO (Maybe [Comment])
getPostComments handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT comment_id \
                       \FROM post_comment \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Comments for Post with id: "
        <> convert postId
        <> " in db!"
      return Nothing
    ids -> do
      Logger.logInfo logh $ "Getting Comments for Post with id: "
        <> convert postId
        <> "."
      let commentIds = map fromSql $ concat ids
      comments <- mapM (DBCo.getComment handle) commentIds
      return $ sequence comments
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPostComments!\n"
            <> show e

getPostAuthorId :: Handle IO -> PostId -> IO (Maybe AuthorId)
getPostAuthorId handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id \
                       \FROM post_author \
                       \WHERE post_id = ?"
       [toSql postId]
  case r of
    [[authorId]] -> do
      Logger.logInfo logh "Getting author_id corresponding to this Post from db."
      return $ Just $ fromSql authorId
    _ -> do
      Logger.logError logh "No Author corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPostAuthorId!\n"
            <> show e

getPostCategoryId :: Handle IO -> PostId -> IO (Maybe CategoryId)
getPostCategoryId handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT category_id \
                       \FROM post_category \
                       \WHERE post_id = ?"
       [toSql postId]
  case r of
    [[catId]] -> do
      Logger.logInfo logh "Getting category_id corresponding to this Post from db."
      return $ Just $ fromSql catId
    _ -> do
      Logger.logError logh "No Category corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPostCategoryId!\n"
            <> show e

getPostTagsIds :: Handle IO -> PostId -> IO (Maybe [TagId])
getPostTagsIds handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT tag_id \
                       \FROM post_tag \
                       \WHERE post_id = ?"
        [toSql postId]
  case r of
    [tagIds] -> do
      Logger.logInfo logh "Getting tag_id corresponding to this Post from db."
      return $ Just $ map fromSql tagIds
    _ -> do
      Logger.logWarning logh "No Tags corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getPostTagsIds!\n"
            <> show e

getPostDraftId :: Handle IO -> PostId -> IO (Maybe DraftId)
getPostDraftId handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id \
                       \FROM post_draft \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [[draftId]] -> do 
      Logger.logInfo logh "Dependency between Post and Draft already exists."
      return $ Just $ fromSql draftId
    _ -> do
      Logger.logError logh "Dependency between Post and Draft doesn't exist."
      return Nothing
  where errorHandler e = do 
          Exc.throwIO $ E.DbError $ "Error: Error in getPostDraftId!\n"
            <> show e

createPostAuthorDep :: Handle IO -> PostId -> AuthorId -> IO ()
createPostAuthorDep handle postId authorId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id \
                       \FROM post_author \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO post_author (post_id, author_id) \
                   \VALUES (?,?)" 
           [toSql postId, toSql authorId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Author."
    _ -> do Logger.logError logh "Dependency between Post and Author already exists."
  where errorHandler e = do 
          Exc.throwIO $ E.DbError $ "Error: Error in createPostAuthorDep!\n"
            <> show e

createPostCatDep :: Handle IO -> PostId -> CategoryId -> IO ()
createPostCatDep handle postId catId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT category_id \
                       \FROM post_category \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO post_category (post_id, category_id) \
                   \VALUES (?,?)" 
           [toSql postId, toSql catId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Category."
    _ -> do Logger.logError logh "Dependency between Post and Category already exists."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in createPostCatDep!\n"
            <> show e

createPostTagDep :: Handle IO -> PostId -> TagId -> IO ()
createPostTagDep handle postId tagId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT tag_id \
                       \FROM post_tag \
                       \WHERE post_id = ? AND tag_id = ?" 
        [toSql postId, toSql tagId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO post_tag (post_id, tag_id) \
                   \VALUES (?,?)" 
            [toSql postId, toSql tagId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Tag."
    _ -> do Logger.logError logh "Dependency between Post and Tag already exists."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in createPostTagDep!\n"
            <> show e

createPostDraftDep :: Handle IO -> PostId -> DraftId -> IO ()
createPostDraftDep handle postId draftId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  draftIdDbMaybe <- getPostDraftId handle postId
  case draftIdDbMaybe of
    Nothing -> do
      _ <- run dbh "INSERT INTO post_draft (post_id, draft_id) \
                   \VALUES (?,?)" 
            [toSql postId, toSql draftId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Draft."
    Just _ -> do Logger.logError logh $ "Post with id: "
                   <> convert postId 
                   <> " already has Draft."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in createPostDraftDep!\n"
            <> show e

removePostAuthorDep :: Handle IO -> PostId -> IO (Maybe AuthorId)
removePostAuthorDep handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id \
                       \FROM post_author \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [[authorId]] -> do
      _ <- run dbh "DELETE FROM post_author \
                   \WHERE post_id = ?"
            [toSql postId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Post and Author."
      return $ Just $ fromSql authorId
    _ -> do
      Logger.logError logh "Dependency between Post and Author doesn't exist."
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePostAuthorDep!\n"
            <> show e

removePostCatDep :: Handle IO -> PostId -> IO (Maybe CategoryId)
removePostCatDep handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id \
                       \FROM post_category \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [[catId]] -> do
      _ <- run dbh "DELETE FROM post_category \
                   \WHERE post_id = ?"
            [toSql postId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Post and Category."
      return $ Just $ fromSql catId
    _ -> do
      Logger.logError logh "Dependency between Post and Category doesn't exist."
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePostCatDep!\n"
            <> show e

removePostTagDep :: Handle IO -> PostId -> IO (Maybe [TagId])
removePostTagDep handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT post_id \
                       \FROM post_tag \
                       \WHERE post_id = ?" 
        [toSql postId]
  case r of
    [tags] -> do
      _ <- run dbh "DELETE FROM post_tag \
                   \WHERE post_id = ?"
            [toSql postId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Post and Tag."
      return $ Just $ map fromSql tags
    _ -> do 
      Logger.logError logh "Dependency between Post and Tag doesn't exist."
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePostTagDep!\n"
            <> show e

removePostMainPhotoDep :: Handle IO -> PostId -> IO (Maybe PhotoId)
removePostMainPhotoDep handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT photo_id \
                       \FROM post_main_photo \
                       \WHERE post_id = ?"
        [toSql postId]
  case r of
    [[photoId]] -> do
      Logger.logInfo logh "Removing dependency between Post and Main Photo from db."
      _ <- run dbh "DELETE FROM post_main_photo \
                   \WHERE post_id = ?"
           [toSql postId]
      commit dbh
      return $ Just $ fromSql photoId
    _ -> do
      Logger.logWarning logh "No Main Photo corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePostMainPhotoDep!\n"
            <> show e

removePostAddPhotoDep :: Handle IO -> PostId -> IO (Maybe [PhotoId])
removePostAddPhotoDep handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT photo_id \
                       \FROM post_add_photo \
                       \WHERE post_id = ?"
        [toSql postId]
  case r of
    [photoIds] -> do
      Logger.logInfo logh "Removing dependency between Post and Additional Photo from db."
      _ <- run dbh "DELETE FROM post_add_photo \
                   \WHERE post_id = ?" 
            [toSql postId]
      commit dbh
      return $ Just $ map fromSql photoIds
    _ -> do
      Logger.logWarning logh "No Additional Photo corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePostAddPhotoDep!\n"
            <> show e

removePostCommentDep :: Handle IO -> PostId -> IO (Maybe [PhotoId])
removePostCommentDep handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT comment_id \
                       \FROM post_comment \
                       \WHERE post_id = ?"
        [toSql postId]
  case r of
    [commentIds] -> do
      Logger.logInfo logh "Removing dependency between Post and Comment from db."
      _ <- run dbh "DELETE FROM post_comment \
                   \WHERE post_id = ?"
            [toSql postId]
      commit dbh
      return $ Just $ map fromSql commentIds
    _ -> do
      Logger.logWarning logh "No Comment corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePostCommentDep!\n"
            <> show e

removePostDraftDep :: Handle IO -> PostId -> IO (Maybe [DraftId])
removePostDraftDep handle postId = handleSql errorHandler $ do
  let dbh = conn handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT comment_id \
                       \FROM post_comment \
                       \WHERE post_id = ?"
        [toSql postId]
  case r of
    [draftIds] -> do
      Logger.logInfo logh "Removing dependency between Post and Draft from db."
      _ <- run dbh "DELETE FROM post_comment \
                   \WHERE post_id = ?"
            [toSql postId]
      commit dbh
      return $ Just $ map fromSql draftIds
    _ -> do
      Logger.logWarning logh "No Draft corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in removePostDraftDep!\n"
            <> show e

newPost :: Handle IO -> [SqlValue] -> IO (Maybe Post)
newPost handle [idPost, title, created_at, text] = do
  let postId = fromSql idPost
      postTitle = fromSql title
      createdAt = fromSql created_at
      body = fromSql text
  photoMainMaybe <- getPostMainPhoto handle postId
  photosAddMaybe <- getPostAddPhotos handle postId
  commentsMaybe <- getPostComments handle postId
  tags <- runMaybeT $ do 
    tagIds <- MaybeT $ getPostTagsIds handle postId
    tags <- MaybeT $ DBT.getTags handle tagIds
    return tags
  runMaybeT $ do
    authorId <- MaybeT $ getPostAuthorId handle postId
    author <- MaybeT $ DBA.getAuthor handle authorId
    catId <- MaybeT $ getPostCategoryId handle postId
    cat <- MaybeT $ DBC.getCat handle catId
    return Post {
      post_id = postId,
      post_title = postTitle,
      post_createdAt = createdAt,
      post_text = body,
      post_mainPhoto = photoMainMaybe,
      post_addPhotos = photosAddMaybe,
      post_comments = commentsMaybe,
      post_tags = tags,
      post_author = author,
      post_category = cat
    }
newPost _ _ = return Nothing