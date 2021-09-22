{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Post where

import Control.Monad (when, liftM, join)
import Database.HDBC (IConnection, SqlValue(..), handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersect, union, intercalate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Maybe (fromMaybe)
--import Data.Time.Clock (UTCTime(..), Day)

import Post.DB.DBSpec (Handle(..), Config(..))
import qualified Post.Logger as Logger
import qualified Post.DB.Author as DBA
import qualified Post.DB.Category as DBC
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Photo as DBPh
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Data as DbData
import Post.Server.Objects
import Post.Server.Util (convert)

-- | DB methods for Post
createPost :: Handle IO -> Title -> Text -> Id -> Id -> [Id] -> IO (Maybe Text)
createPost handle title text authorId catId tagIds = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM posts WHERE title = ?" [toSql title]
  case r of
    [] -> do
      time <- getCurrentTime
      --let createdAt = utctDay time
      _ <- run dbh "INSERT INTO posts (title, text, created_at) VALUES (?,?,?)"
            [toSql title, toSql text, toSql time]
      commit dbh
      Logger.logInfo logh $ "Post with title: " <> title <> " was successfully inserted in db."
      r <- quickQuery' dbh "SELECT id FROM posts ORDER BY id DESC LIMIT 1" []
      case r of
        [] -> do
          Logger.logError logh "Error while inserting Post to db."
          return $ Just "Error while inserting Post to db."
        [[postId]] -> do
          createPostAuthorDep handle (fromSql postId :: Integer) authorId
          createPostCatDep handle (fromSql postId :: Integer) catId
          mapM_ (createPostTagDep handle (fromSql postId :: Integer)) tagIds
          --createPostMainPhotoDep handle (fromSql postId :: Integer) tagsId
          --createPostAddPhotoDep handle (fromSql postId :: Integer) tagsId
          return Nothing
        _ -> do
          Logger.logWarning logh $ "Post with title: " <> title <> " already exists in db."
          return $ Just $ "Post with title: " <> title <> " already exists."
  where errorHandler e = do fail $ "Error: Error in createPost!\n" <> show e

getPosts :: Handle IO -> DbData.DbReq -> IO (Maybe [Post])
getPosts handle dbReq = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  -- Request to DB table posts
  let dbPostQuery = "SELECT id FROM posts " <> fst (DbData.dbPostReq dbReq)
      dbPostArgs = snd $ DbData.dbPostReq dbReq
  Logger.logDebug logh $ "Query to table 'posts': " <> T.pack dbPostQuery
  rPost <- quickQuery' dbh dbPostQuery dbPostArgs
  -- Request to DB table post_category
  let dbCatQuery = "SELECT post_id FROM post_category " <> fst (DbData.dbCatReq dbReq)
      dbCatArgs = snd $ DbData.dbCatReq dbReq
  Logger.logDebug logh $ "Query to table 'post_category': " <> T.pack dbCatQuery
  rCat <- quickQuery' dbh dbCatQuery dbCatArgs
  -- Request to DB table post_tag
  let dbTagQuery = "SELECT post_id FROM post_tag " <> fst (DbData.dbTagReq dbReq)
      dbTagArgs = snd $ DbData.dbTagReq dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': " <> T.pack dbTagQuery
  rTag <- quickQuery' dbh dbTagQuery dbTagArgs
  -- Request to DB table post_author
  let dbAuthorQuery = "SELECT post_id FROM post_author " <> fst (DbData.dbAuthorReq dbReq)
      dbAuthorArgs = snd $ DbData.dbAuthorReq dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': " <> T.pack dbAuthorQuery
  rAuthor <- quickQuery' dbh dbAuthorQuery dbAuthorArgs
  -- Serch request to DB table posts
  let dbPostSearchQuery = "SELECT id FROM posts " <> fst (DbData.dbPostSearch dbReq)
      dbPostSearchArgs = snd $ DbData.dbPostSearch dbReq
  Logger.logDebug logh $ "Search Query to table 'posts': " <> T.pack dbPostSearchQuery
  rPostSearch <- quickQuery' dbh dbPostSearchQuery dbPostSearchArgs
  -- Serch request to DB table post_author
  let dbAuthorSearchQuery = "SELECT post_id FROM post_author " <> fst (DbData.dbAuthorSearch dbReq)
      dbAuthorSearchArgs = snd $ DbData.dbAuthorSearch dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': " <> T.pack dbAuthorSearchQuery
  rAuthorSearch <- quickQuery' dbh dbAuthorSearchQuery dbAuthorSearchArgs
  -- Serch request to DB table post_category
  let dbCatSearchQuery = "SELECT post_id FROM post_category " <> fst (DbData.dbCatSearch dbReq)
      dbCatSearchArgs = snd $ DbData.dbCatSearch dbReq
  Logger.logDebug logh $ "Query to table 'post_category': " <> T.pack dbCatSearchQuery
  rCatSearch <- quickQuery' dbh dbCatSearchQuery dbCatSearchArgs
  -- Serch request to DB table post_tag
  let dbTagSearchQuery = "SELECT post_id FROM post_tag " <> fst (DbData.dbTagSearch dbReq)
      dbTagSearchArgs = snd $ DbData.dbTagSearch dbReq
  Logger.logDebug logh $ "Query to table 'post_tag': " <> T.pack dbTagSearchQuery
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
      rMain <- quickQuery' dbh (DbData.orderQuery dbReq <> intercalate "," (replicate nAll "?") <> DbData.orderBy dbReq) idAll
      case rAll of
        [] -> do
          Logger.logWarning logh "No posts in db!"
          return Nothing
        ids -> do
          posts <- mapM (getPost handle . (\ x -> fromSql x :: Integer)) (concat ids)
          return $ sequence posts
  where errorHandler e = do fail $ "Error: Error in getPosts!\n" <> show e

getPost :: Handle IO -> Integer -> IO (Maybe Post)
getPost handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id, title, created_at, text FROM posts WHERE id = ?" [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No post with id: " <> convert postId  <> " in db!"
      return Nothing
    [params@[id, title, created_at, text]] -> do
      authorId <- getPostAuthorId handle postId
      checkAuthor <- DBA.getAuthor handle (fromMaybe (-1) authorId)
      let author = fromMaybe author0 checkAuthor
      catId <- getPostCategoryId handle postId
      checkCat <- DBC.getCat handle (fromMaybe (-1) catId)
      let cat = fromMaybe cat0 checkCat
      Logger.logInfo logh "Getting Post from db."
      --tagIds <- getRelatedTagsId handle postId
      --tags <- map (DBT.getTag dbh logh) $ fromMaybe [(-1)] tagIds
      Just <$> newPost params author cat
  where errorHandler e = do fail $ "Error: Error in getPost!\n" <> show e
        newPost [id, title, created_at, text] author cat = do
          photoMainMaybe <- getPostMainPhoto handle (fromSql id :: Integer)
          photosAddMaybe <- getPostAddPhotos handle (fromSql id :: Integer)
          commentsMaybe <- getPostComments handle (fromSql id :: Integer)
          return Post {
            post_title = fromSql title :: Text,
            post_createdAt = fromSql created_at :: Text,
            post_category = cat,
            post_tags = Nothing,
            post_text = fromSql text :: Text,
            post_mainPhoto = photoMainMaybe,
            post_addPhotos = photosAddMaybe,
            post_id = fromSql id :: Integer,
            post_author = author,
            post_comments = commentsMaybe
          }

removePost :: Handle IO -> Id -> IO (Maybe Text)
removePost handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT id FROM posts WHERE user_id = ?" [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Post with id: " <> convert postId <>  "!"
      return $ Just $ "No exists Post with id: " <> convert postId <>  "!"
    _ -> do
      _ <- run dbh "DELETE FROM posts WHERE id = ?" [toSql postId]
      removePostAuthorDep handle postId
      removePostCatDep handle postId
      removePostTagDep handle postId
      --removePostMainPhotoDep handle postId
      --removePostAddPhotoDep handle postId
      --removePostCommentDep handle postId
      --removePostDraftDep handle postId
      commit dbh
      Logger.logInfo logh $ "Removing Post with id: " <> convert postId <> " from db."
      return Nothing
  where errorHandler e = do fail $ "Error: Error in removePost!\n" <> show e

setPostMainPhoto :: Handle IO -> Id -> Text -> IO (Maybe Text)
setPostMainPhoto handle postId path = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  photoIdMaybe <- DBPh.savePhoto handle path
  case photoIdMaybe of
    Nothing -> do
      Logger.logError logh $ "Couldn't set Main Photo for Post with id: " <> convert postId
      return $ Just $ "Couldn't set Main Photo for Post with id: " <> convert postId
    Just photoId -> do
      r <- quickQuery' dbh "SELECT photo_id FROM post_main_photo WHERE post_id = ?" 
            [toSql postId]
      case r of
        [] -> do
          _ <- run dbh "INSERT INTO post_main_photo (photo_id, post_id) VALUES (?,?)" 
                [toSql photoId, toSql postId]
          commit dbh
          Logger.logInfo logh "Post's Main Photo was successfully set."
          return Nothing
        _ -> do
          Logger.logError logh $ "Main Photo for Post with id: " <> convert postId <> " already exists in db."
          return $ Just $ "Main Photo for Post with id: " <> convert postId <> " already exists."
  where errorHandler e = do fail $ "Error: Error in setPostMainPhoto!\n" <> show e

setPostAddPhoto :: Handle IO -> Id -> Text -> IO (Maybe Text)
setPostAddPhoto handle postId path = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  photoIdMaybe <- DBPh.savePhoto handle path
  case photoIdMaybe of
    Nothing -> do
      Logger.logError logh $ "Couldn't set Additional Photo for Post with id: " <> convert postId
      return $ Just $ "Couldn't set Additional Photo for Post with id: " <> convert postId
    Just photoId -> do
      r <- quickQuery' dbh "SELECT photo_id FROM post_add_photo WHERE post_id = ? AND photo_id = ?" 
           [toSql postId, toSql photoId]
      case r of
        [] -> do
          _ <- run dbh "INSERT INTO post_add_photo (photo_id, post_id) VALUES (?,?)" 
                [toSql photoId, toSql postId]
          commit dbh
          Logger.logInfo logh "Post's Add Photo was successfully set."
          return Nothing
        _ -> do
          Logger.logError logh $ "Add Photo with id: " <> convert photoId <> " for Post with id: " <> convert postId <> " already exists in db."
          return $ Just $ "Main Photo with id: " <> convert photoId <> " for Post with id: " <> convert postId <> " already exists."
  where errorHandler e = do fail $ "Error: Error in setPostAddPhoto!\n" <> show e

getPostMainPhoto :: Handle IO -> Id -> IO (Maybe Photo)
getPostMainPhoto handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT photo_id FROM post_main_photo WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Main Photo for Post with id: " <> convert postId <> " in db!"
      return Nothing
    [[photoId]] -> do
      Logger.logInfo logh $ "Getting Main Photo for Post with id: " <> convert postId <> "."
      DBPh.getPhoto handle (fromSql photoId :: Integer)
  where errorHandler e = do fail $ "Error: Error in getPostMainPhoto!\n" <> show e

getPostAddPhotos :: Handle IO -> Id -> IO (Maybe [Photo])
getPostAddPhotos handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT photo_id FROM post_add_photo WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Add Photos for Post with id: " <> convert postId <> " in db!"
      return Nothing
    ids -> do
      Logger.logInfo logh $ "Getting Add Photos for Post with id: " <> convert postId <> "."
      let addPhotoIds = map (\x -> fromSql x :: Integer) $ concat ids
      addPhotos <- mapM (DBPh.getPhoto handle) addPhotoIds
      return $ sequence addPhotos
  where errorHandler e = do fail $ "Error: Error in getPostAddPhotos!\n" <> show e

getPostComments :: Handle IO -> Id -> IO (Maybe [Comment])
getPostComments handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT comment_id FROM post_comment WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logWarning logh $ "No exists Comments for Post with id: " <> convert postId <> " in db!"
      return Nothing
    ids -> do
      Logger.logInfo logh $ "Getting Comments for Post with id: " <> convert postId <> "."
      let commentIds = map (\x -> fromSql x :: Integer) $ concat ids
      comments <- mapM (DBCo.getComment handle) commentIds
      return $ sequence comments
  where errorHandler e = do fail $ "Error: Error in getPostAddPhotos!\n" <> show e

getPostAuthorId :: Handle IO -> Id -> IO (Maybe Id)
getPostAuthorId handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM post_author WHERE post_id = ?" [toSql postId]
  case r of
    [[x]] -> do
      Logger.logInfo logh "Getting author_id corresponding to this Post from db."
      return $ Just $ fromSql x
    _ -> do
      Logger.logError logh "No Author corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getPostAuthorId!\n" <> show e

getPostCategoryId :: Handle IO -> Id -> IO (Maybe Id)
getPostCategoryId handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT category_id FROM post_category WHERE post_id = ?" [toSql postId]
  case r of
    [[x]] -> do
      Logger.logInfo logh "Getting category_id corresponding to this Post from db."
      return $ Just $ fromSql x
    _ -> do
      Logger.logError logh "No Category corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getPostCategoryId!\n" <> show e

getPostTagsIds :: Handle IO -> Id -> IO (Maybe [Id])
getPostTagsIds handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT user_id FROM post_tags WHERE post_id = ?" [toSql postId]
  case r of
    [xs] -> do
      Logger.logInfo logh "Getting tag_id corresponding to this Post from db."
      return $ Just $ map fromSql xs
    _ -> do
      Logger.logError logh "No Tags corresponding to this Post in db!"
      return Nothing
  where errorHandler e = do fail $ "Error: Error in getPostTagsIds!\n" <> show e

getPostDraftId :: Handle IO -> Id -> IO (Maybe Id, Text)
getPostDraftId handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      Logger.logInfo logh "Dependency between Post and Draft doesn't exist."
      return (Nothing, "Dependency between Post and Draft already exists.")
    [[draftId]] -> do 
      Logger.logError logh "Dependency between Post and Draft already exists."
      return (Just (fromSql draftId :: Integer), "Dependency between Post and Draft already exists.")
  where errorHandler e = do fail $ "Error: Error in getPostDraftId!\n" <> show e

createPostAuthorDep :: Handle IO -> Id -> Id -> IO ()
createPostAuthorDep handle postId authorId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM post_author WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO post_author (post_id, author_id) VALUES (?,?)" 
           [toSql postId, toSql authorId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Author."
    _ -> do Logger.logError logh "Dependency between Post and Author already exists."
  where errorHandler e = do fail $ "Error: Error in createPostAuthorDep!\n" <> show e

createPostCatDep :: Handle IO -> Id -> Id -> IO ()
createPostCatDep handle postId catId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT category_id FROM post_category WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO post_category (post_id, category_id) VALUES (?,?)" 
           [toSql postId, toSql catId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Category."
    _ -> do Logger.logError logh "Dependency between Post and Category already exists."
  where errorHandler e = do fail $ "Error: Error in createPostCatDep!\n" <> show e

createPostTagDep :: Handle IO -> Id -> Id -> IO ()
createPostTagDep handle postId tagId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT tag_id FROM post_tag WHERE post_id = ? AND tag_id = ?" 
        [toSql postId, toSql tagId]
  case r of
    [] -> do
      _ <- run dbh "INSERT INTO post_tag (post_id, tag_id) VALUES (?,?)" 
            [toSql postId, toSql tagId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Tag."
    _ -> do Logger.logError logh "Dependency between Post and Tag already exists."
  where errorHandler e = do fail $ "Error: Error in createPostTagDep!\n" <> show e

createPostDraftDep :: Handle IO -> Id -> Id -> IO ()
createPostDraftDep handle postId draftId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  (draftIdDbMaybe, msg) <- getPostDraftId handle postId
  case draftIdDbMaybe of
    Nothing -> do
      _ <- run dbh "INSERT INTO post_draft (post_id, draft_id) VALUES (?,?)" 
            [toSql postId, toSql draftId]
      commit dbh
      Logger.logInfo logh "Creating dependency between Post and Draft."
    Just _ -> do Logger.logError logh msg
  where errorHandler e = do fail $ "Error: Error in createPostDraftDep!\n" <> show e

removePostAuthorDep :: Handle IO -> Id -> IO ()
removePostAuthorDep handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM post_author WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do Logger.logError logh "Dependency between Post and Author doesn't exist."
    _ -> do
      _ <- run dbh "DELETE FROM post_author WHERE post_id = ?" [toSql postId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Post and Author."
  where errorHandler e = do fail $ "Error: Error in removePostAuthorDep!\n" <> show e

removePostCatDep :: Handle IO -> Id -> IO ()
removePostCatDep handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM post_category WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do Logger.logError logh "Dependency between Post and Category doesn't exist."
    _ -> do
      _ <- run dbh "DELETE FROM post_category WHERE post_id = ?" [toSql postId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Post and Category."
  where errorHandler e = do fail $ "Error: Error in removePostCatDep!\n" <> show e

removePostTagDep :: Handle IO -> Id -> IO ()
removePostTagDep handle postId = handleSql errorHandler $ do
  let dbh = hDB handle
      logh = hLogger handle
  r <- quickQuery' dbh "SELECT author_id FROM post_tag WHERE post_id = ?" 
        [toSql postId]
  case r of
    [] -> do Logger.logError logh "Dependency between Post and Tag doesn't exist."
    _ -> do
      _ <- run dbh "DELETE FROM post_tag WHERE post_id = ?" [toSql postId]
      commit dbh
      Logger.logInfo logh "Removing dependency between Post and Tag."
  where errorHandler e = do fail $ "Error: Error in removePostTagDep!\n" <> show e

user0 = User {
  user_id = 1010, -- Unique identifier for this User.
  user_isAdmin = False, -- True, if this user is a Admin.
  user_firstName = "Jhon", -- User's first name.
  user_lastName = "Thomson", -- User's last name.
  user_photo = Nothing
}

author0 = Author {
  author_user = user0, -- User.
  author_description = "Adventures" -- Author's description.
}

cat0 = Category {
  category_id = 0, -- Unique identifier for this Category.
  category_title = "Sport", -- Title of Category.
  category_subcategory = Just cat1
}

cat1 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit", -- Title of Category.
  category_subcategory = Just cat2
}

cat2 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit Games", -- Title of Category.
  category_subcategory = Nothing
}

tag0 = Tag {
  tag_id = 0,
  tag_title = "sport"
}

tag1 = Tag {
  tag_id = 1,
  tag_title = "crossfit"
}

post0 = Post {
  post_id = 0, -- Unique identifier for this Announcement.
  post_author = author0 , -- Author of Announcement.
  post_title = "Crossfit Games 2021", -- Title of Announcement.
  post_createdAt = "03.08.21", -- Date when the Announcement was created.
  post_category = cat0, -- Category of Announcement.
  post_tags = Just [tag0,tag1], -- Array of Tag of Announcement.
  post_text = "Yesterday was the last day of competitions of the 2021 Crossfit Games", -- Text of Announcement.
  post_mainPhoto = Nothing, -- Main Photo of Announcement.
  post_addPhotos = Nothing, -- Array of Additional Photos of Announcement.
  post_comments = Just [com0, com1] -- Array of Comments of Announcement.
}

com0 = Comment {
  comment_id = 2, -- Unique identifier for this Comment.
  comment_text = "Happy!"
}

com1 = Comment {
  comment_id = 3, -- Unique identifier for this Comment.
  comment_text = "Very Happy!"
}

draft0 = Draft {
  draft_id = 0 , -- Unique identifier for this Draft.
  draft_text = "No text"
}