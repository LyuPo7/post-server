{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Post where

import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Text (Text)
import Data.List (intersect)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Data.Either.Combinators (rightToMaybe)

import Post.DB.DBQSpec
import qualified Post.Logger as Logger
import qualified Post.DB.Author as DBA
import qualified Post.DB.Category as DBC
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Photo as DBPh
import qualified Post.DB.Comment as DBCo
import Post.Server.Objects
import Post.DB.Data
import Post.Server.Util (convert)

-- | DB methods for Post
createPost :: Monad m => Handle m -> Title ->
              Text -> AuthorId -> CategoryId -> [TagId] -> m (Either Text PostId)
createPost handle title text authorId catId tagIds = do
  let logh = hLogger handle
  postIdE <- getPostIdRecordByTitle handle title
  case postIdE of
    Left _ -> runEitherT $ do
      _ <- EitherT $ DBC.getCatRecordByCatId handle catId
      _ <- EitherT $ DBT.getTagRecordsByIds handle tagIds
      lift $ insertPostRecord handle title text
      postId <- EitherT $ getLastPostRecord handle
      _ <- EitherT $ createPostAuthorDep handle postId authorId
      _ <- EitherT $ createPostCatDep handle postId catId
      lift $ mapM_ (createPostTagDep handle postId) tagIds
      return postId
    Right _ -> do
      let msg = "Post with title: '"
            <> title
            <> "' already exists in db."
      Logger.logWarning logh msg
      return $ Left msg

getPosts :: Monad m => Handle m -> PostQuery -> m (Either Text [Post])
getPosts handle postQuery = do
  let logh = hLogger handle
  -- Request to DB table posts
  idAllPosts <- searchPost handle postQuery
  idCatPosts <- searchCat handle postQuery
  idTagPosts <- searchTag handle postQuery
  idAuthorPosts <- searchAuthor handle postQuery
  -- Search request to DB table posts
  idSearch <- findIn handle postQuery
  -- Requests
  let idAllSimple = ((idAllPosts 
        `intersect` idCatPosts) 
        `intersect` idTagPosts) 
        `intersect` idAuthorPosts
      idAll = idAllSimple `intersect` idSearch
  -- Request with sorting
  case idAll of
    [] -> do
      Logger.logWarning logh "No Posts in db!"
      return $ Left "No Posts!"
    correctIds -> do
      Logger.logInfo logh "Sorting Posts from db!"
      postsSql <- sortQuery handle postQuery $ map toSql correctIds
      case postsSql of
        [] -> do
          Logger.logWarning logh "No Posts in db!"
          return $ Left "No Posts!"
        ids -> do
          posts <- mapM (getPostRecord handle) ids
          return $ sequence posts

removePost :: Monad m => Handle m -> PostId -> m (Either Text PostId)
removePost handle postId = runEitherT $ do
  _ <- EitherT $ getPostRecord handle postId
  _ <- lift $ removePostAuthorDep handle postId
  _ <- lift $ removePostCatDep handle postId
  _ <- lift $ removePostTagDep handle postId
  _ <- lift $ removePostMainPhotoDep handle postId
  _ <- lift $ removePostAddPhotoDep handle postId
  _ <- lift $ removePostCommentDep handle postId
  _ <- lift $ removePostDraftDep handle postId
  _ <- lift $ deletePostRecord handle postId
  return postId

-- | DB methods for Post
setPostMainPhoto :: Monad m => Handle m -> PostId -> Text -> m (Either Text PhotoId)
setPostMainPhoto handle postId path = do
  let logh = hLogger handle
  photoIdE <- DBPh.savePhoto handle path
  case photoIdE of
    Left _ -> do
      let msg = "Couldn't set Main Photo for Post with id: "
            <> convert postId
      Logger.logError logh msg
      return $ Left msg
    Right photoId -> do
      oldPhotoIdE <- getPostMainPhotoRecords handle postId
      case oldPhotoIdE of
        Left _ -> insertPostMainPhotoRecord handle postId photoId
        Right _ -> updatePostMainPhotoRecord handle postId photoId

setPostAddPhoto :: Monad m => Handle m -> PostId -> Text -> m (Either Text PhotoId)
setPostAddPhoto handle postId path = do
  let logh = hLogger handle
  photoIdE <- DBPh.savePhoto handle path
  case photoIdE of
    Left _ -> do
      let msg = "Couldn't set Additional Photo for Post with id: "
            <> convert postId
      Logger.logError logh msg
      return $ Left msg
    Right photoId -> insertPostAddPhotoRecord handle postId photoId

createPostAuthorDep :: Monad m => Handle m ->
                       PostId -> AuthorId -> m (Either Text AuthorId)
createPostAuthorDep handle postId authorId = do
  let logh = hLogger handle
  postAuthorDepE <- getPostAuthorRecord handle postId
  case postAuthorDepE of
    Left _ -> do
      _ <- insertPostAuthorRecord handle postId authorId
      return $ Right authorId
    Right _ -> do
      let msg = "Dependency between Post and Author already exists."
      Logger.logError logh msg 
      return $ Left msg

createPostCatDep :: Monad m => Handle m ->
                    PostId -> CategoryId -> m (Either Text CategoryId)
createPostCatDep handle postId catId = do
  let logh = hLogger handle
  postCatDepE <- getPostCategoryRecord handle postId
  case postCatDepE of
    Left _ -> do
      _ <- insertPostCatRecord handle postId catId
      return $ Right catId
    Right _ -> do
      let msg = "Dependency between \
                \Post and Category already exists."
      Logger.logError logh msg
      return $ Left msg

createPostTagDep :: Monad m => Handle m -> PostId -> TagId -> m (Either Text TagId)
createPostTagDep handle postId tagId = do
  let logh = hLogger handle
  tagsIdE <- getPostTagRecords handle postId
  case tagsIdE of
    Left _ -> insertPostTagRecord handle postId tagId
    Right tags -> do
      if tagId `elem` tags
        then do
          let msg = "Dependency between \
                    \Post and Tag already exists."
          Logger.logWarning logh msg
          return $ Left msg
        else do
          Logger.logInfo logh "Inserting dependency between Post and Tag in db."
          insertPostTagRecord handle postId tagId

createPostDraftDep :: Monad m => Handle m ->
                      PostId -> DraftId -> m (Either Text DraftId)
createPostDraftDep handle postId draftId = do
  let logh = hLogger handle
  draftIdDbE <- getPostDraftRecord handle postId
  case draftIdDbE of
    Left _ -> insertPostDraftRecord handle postId draftId
    Right _ -> do
      let msg = "Draft for Post with id: "
            <> convert postId
            <> " already exists in db!"
      Logger.logError logh msg
      return $ Left msg

removePostAuthorDep :: Monad m => Handle m -> PostId -> m (Either Text AuthorId)
removePostAuthorDep handle postId = runEitherT $ do
  authorId <- EitherT $ getPostAuthorRecord handle postId
  lift $ deletePostAuthorRecord handle postId
  return authorId

removePostCatDep :: Monad m => Handle m -> PostId -> m (Either Text CategoryId)
removePostCatDep handle postId = runEitherT $ do
  catId <- EitherT $ getPostCategoryRecord handle postId
  lift $ deletePostCatRecord handle postId
  return catId

removePostTagDep :: Monad m => Handle m -> PostId -> m (Either Text [TagId])
removePostTagDep handle postId = runEitherT $ do
  tagsId <- EitherT $ getPostTagRecords handle postId
  lift $ deletePostTagRecord handle postId
  return tagsId

removePostMainPhotoDep :: Monad m => Handle m -> PostId -> m (Either Text PhotoId)
removePostMainPhotoDep handle postId = runEitherT $ do
  photoId <- EitherT $ getPostMainPhotoRecords handle postId
  lift $ deletePostMainPhotoRecord handle postId
  return $ photo_id photoId

removePostAddPhotoDep :: Monad m => Handle m -> PostId -> m (Either Text [PhotoId])
removePostAddPhotoDep handle postId = runEitherT $ do
  photosAdd <- EitherT $ getPostAddPhotoRecords handle postId
  lift $ deletePostAddPhotoRecords handle postId
  return $ map photo_id photosAdd

removePostCommentDep :: Monad m => Handle m -> PostId -> m (Either Text [PhotoId])
removePostCommentDep handle postId = runEitherT $ do
  comments <- EitherT $ getPostCommentRecords handle postId
  lift $ deletePostComRecords handle postId
  return $ map comment_id comments    

removePostDraftDep :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
removePostDraftDep handle postId = runEitherT $ do
  draftId <- EitherT $ getPostDraftRecord handle postId
  lift $ deletePostDraftRecord handle postId
  return draftId

getPostRecord :: Monad m => Handle m -> PostId -> m (Either Text Post)
getPostRecord handle postId = do
  let logh = hLogger handle
  postSQL <- selectFromWhere handle tablePosts
             [colIdPost, colTitlePost, colCreatedAtPost, colTextPost]
             [colIdPost]
             [toSql postId]
  case postSQL of
    [post] -> do
      Logger.logInfo logh "Getting Post from db."
      newPost handle post
    _ -> do
      let msg = "No Post with id: "
            <> convert postId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg

getLastPostRecord :: Monad m => Handle m -> m (Either Text PostId)
getLastPostRecord handle = do
  let logh = hLogger handle
  idPostSql <- selectFromOrderLimit handle tablePosts
                [colIdPost]
                 colIdPost 1
  case idPostSql of
    [[idPost]] -> do
      let postId = fromSql idPost
      Logger.logInfo logh $ "Last Post inserted in db with id: "
        <> convert postId
      return $ Right postId
    _ -> do
      let msg = "No exist Posts in db!"
      Logger.logWarning logh msg
      return $ Left msg

getPostIdRecordByTitle :: Monad m => Handle m -> Title -> m (Either Text PostId)
getPostIdRecordByTitle handle title = do
  let logh = hLogger handle
  postIdSQL <- selectFromWhere handle tablePosts
             [colIdPost]
             [colTitlePost]
             [toSql title]
  case postIdSQL of
    [[idPost]] -> do
      Logger.logInfo logh $ "Getting PostId corresponding to title: '"
        <> title
        <> "' from db."
      return $ Right $ fromSql idPost
    _ -> do
      let msg = "No exists Post with title: "
            <> title
            <> " in db!"
      Logger.logError logh msg
      return $ Left msg

getPostAuthorRecord :: Monad m => Handle m -> PostId -> m (Either Text AuthorId)
getPostAuthorRecord handle postId = do
  let logh = hLogger handle
  authorIdSql <- selectFromWhere handle tablePostAuthor
               [colIdAuthorPostAuthor]
               [colIdPostPostAuthor]
               [toSql postId]
  case authorIdSql of
    [[authorId]] -> do
      Logger.logInfo logh $ "Getting AuthorId \
           \corresponding to Post with id: "
        <> convert postId
        <> " from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg = "No Author corresponding to Post with id: "
            <> convert postId
            <> "in db!"
      Logger.logInfo logh msg
      return $ Left msg

getPostCategoryRecord :: Monad m => Handle m -> PostId -> m (Either Text CategoryId)
getPostCategoryRecord handle postId = do
  let logh = hLogger handle
  catIdSql <- selectFromWhere handle tablePostCat
               [colIdCatPostCat]
               [colIdPostPostCat]
               [toSql postId]
  case catIdSql of
    [[catId]] -> do
      Logger.logInfo logh $ "Getting CategoryId \
           \corresponding to Post with id: "
        <> convert postId
        <> " from db."
      return $ Right $ fromSql catId
    _ -> do
      let msg = "No exists Category corresponding to Post with id: "
            <> convert postId
            <> "in db!"
      Logger.logInfo logh msg
      return $ Left msg

getPostTagRecords :: Monad m => Handle m -> PostId -> m (Either Text [TagId])
getPostTagRecords handle postId = do
  let logh = hLogger handle
  tagsIdSql <- selectFromWhere handle tablePostTag
               [colIdTagPostTag]
               [colIdPostPostTag]
               [toSql postId]
  case tagsIdSql of
    [tagIds] -> do
      Logger.logInfo logh $ "Getting TagId \
           \corresponding to Post with id: "
        <> convert postId
        <> " from db."
      return $ Right $ map fromSql tagIds
    _ -> do
      let msg = "No exist Tags corresponding to Post with id: "
            <> convert postId
            <> " in db!"
      Logger.logInfo logh msg
      return $ Left msg

getPostMainPhotoRecords :: Monad m => Handle m -> PostId -> m (Either Text Photo)
getPostMainPhotoRecords handle postId = do
  let logh = hLogger handle
  photoIdSql <- selectFromWhere handle tablePostMainPhoto
               [colIdPhotoPostMainPhoto]
               [colIdPostPostMainPhoto]
               [toSql postId]
  case photoIdSql of
    [[photoId]] -> do
      Logger.logInfo logh $ "Getting Main Photo for Post with id: "
        <> convert postId <> "."
      DBPh.getPhotoRecordById handle $ fromSql photoId
    _ -> do
      let msg = "No exists Main Photo for Post with id: "
            <> convert postId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg

getPostAddPhotoRecords :: Monad m => Handle m -> PostId -> m (Either Text [Photo])
getPostAddPhotoRecords handle postId = do
  let logh = hLogger handle
  photoIdSql <- selectFromWhere handle tablePostAddPhoto
               [colIdPhotoPostAddPhoto]
               [colIdPostPostAddPhoto]
               [toSql postId]
  case photoIdSql of
    [] -> do
      let msg = "No exist Add Photos for Post with id: "
            <> convert postId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    ids -> do
      Logger.logInfo logh $ "Getting Add Photos for Post with id: "
        <> convert postId
        <> " from db."
      let addPhotoIds = map fromSql $ concat ids
      addPhotos <- mapM (DBPh.getPhotoRecordById handle) addPhotoIds
      return $ sequence addPhotos

getPostDraftRecord :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
getPostDraftRecord handle postId = do
  let logh = hLogger handle
  draftIdSql <- selectFromWhere handle tablePostDraft
               [colIdDraftPostDraft]
               [colIdPostPostDraft]
               [toSql postId]
  case draftIdSql of
    [[draftId]] -> do 
      Logger.logInfo logh "Dependency between Post and Draft already exists."
      return $ Right $ fromSql draftId
    _ -> do
      let msg = "Dependency between Post and Draft doesn't exist."
      Logger.logError logh msg
      return $ Left msg

getPostCommentRecords :: Monad m => Handle m -> PostId -> m (Either Text [Comment])
getPostCommentRecords handle postId = do
  let logh = hLogger handle
  comsIdSql <- selectFromWhere handle tablePostCom
               [colIdComPostCom]
               [colIdPostPostCom]
               [toSql postId]
  case comsIdSql of
    [] -> do
      let msg = "No exist Comments for Post with id: "
            <> convert postId
            <> " in db!"
      Logger.logWarning logh msg
      return $ Left msg
    ids -> do
      Logger.logInfo logh $ "Getting Comments for Post with id: "
        <> convert postId
        <> " from db."
      let commentIds = map fromSql $ concat ids
      comments <- mapM (DBCo.getCommentRecord handle) commentIds
      return $ sequence comments

insertPostRecord :: Monad m => Handle m -> Title -> Text -> m ()
insertPostRecord handle title text = do
  let logh = hLogger handle
  time <- getCurrentTime handle
  _ <- insertIntoValues handle tablePosts
        [colTitlePost, colTextPost, colCreatedAtPost]
        [toSql title, toSql text, toSql time]
  Logger.logInfo logh $ "Post with title: '"
    <> title
    <> "' was successfully inserted in db."

updatePostMainPhotoRecord :: Monad m => Handle m ->
                             PostId -> PhotoId -> m (Either Text PhotoId)
updatePostMainPhotoRecord handle postId photoId = do
  let logh = hLogger handle
  _ <- updateSetWhere handle tablePostMainPhoto
           [colIdPhotoPostMainPhoto]
           [colIdPostPostMainPhoto]
           [toSql photoId]
           [toSql postId]
  Logger.logInfo logh "Post's Main Photo was successfully set."
  return $ Right photoId

insertPostMainPhotoRecord :: Monad m => Handle m ->
                             PostId -> PhotoId -> m (Either Text PhotoId)
insertPostMainPhotoRecord handle postId photoId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostMainPhoto
        [colIdPhotoPostMainPhoto, colIdPostPostMainPhoto] 
        [toSql photoId, toSql postId]
  Logger.logInfo logh "Post's Main Photo was successfully updated."
  return $ Right photoId

insertPostAddPhotoRecord :: Monad m => Handle m ->
                            PostId -> PhotoId -> m (Either Text PhotoId)
insertPostAddPhotoRecord handle postId photoId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostAddPhoto
        [colIdPhotoPostAddPhoto, colIdPostPostAddPhoto] 
        [toSql photoId, toSql postId]
  Logger.logInfo logh "Post's Add Photo was successfully inserted in db."
  return $ Right photoId

insertPostAuthorRecord :: Monad m => Handle m -> PostId -> AuthorId -> m ()
insertPostAuthorRecord handle postId authorId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostAuthor 
        [colIdPostPostAuthor, colIdAuthorPostAuthor] 
        [toSql postId, toSql authorId]
  Logger.logInfo logh "Creating dependency between Post and Author."

insertPostCatRecord :: Monad m => Handle m -> PostId -> CategoryId -> m ()
insertPostCatRecord handle postId catId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostCat 
        [colIdPostPostCat, colIdCatPostCat] 
        [toSql postId, toSql catId]
  Logger.logInfo logh "Creating dependency between Post and Category."

insertPostTagRecord :: Monad m => Handle m -> PostId -> TagId -> m (Either Text TagId)
insertPostTagRecord handle postId tagId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostTag
        [colIdTagPostTag, colIdPostPostTag] 
        [toSql tagId, toSql postId]
  Logger.logInfo logh "Creating dependency between Post and Tag."
  return $ Right tagId

insertPostDraftRecord :: Monad m => Handle m ->
                         PostId -> DraftId -> m (Either Text DraftId)
insertPostDraftRecord handle postId draftId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostDraft
        [colIdDraftPostDraft, colIdPostPostDraft] 
        [toSql draftId, toSql postId]
  Logger.logInfo logh "Creating dependency between Post and Draft."
  return $ Right draftId

deletePostRecord :: Monad m => Handle m -> PostId -> m ()
deletePostRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePosts
        [colIdPost]
        [toSql postId]
  Logger.logInfo logh $ "Removing Post with id: "
    <> convert postId
    <> " from db."

deletePostAuthorRecord :: Monad m => Handle m -> PostId -> m ()
deletePostAuthorRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostAuthor
        [colIdPostPostAuthor]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Author."

deletePostCatRecord :: Monad m => Handle m -> PostId -> m ()
deletePostCatRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostCat
        [colIdPostPostCat]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Category."

deletePostTagRecord :: Monad m => Handle m -> PostId -> m ()
deletePostTagRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostTag
        [colIdPostPostTag]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Tag."

deletePostMainPhotoRecord :: Monad m => Handle m -> PostId -> m ()
deletePostMainPhotoRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostMainPhoto
        [colIdPostPostMainPhoto]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Main Photo from db."

deletePostAddPhotoRecords :: Monad m => Handle m -> PostId -> m ()
deletePostAddPhotoRecords handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostAddPhoto
        [colIdPostPostAddPhoto]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between \
                      \Post and Additional Photo from db."

deletePostComRecords :: Monad m => Handle m -> PostId -> m ()
deletePostComRecords handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostCom
        [colIdPostPostCom]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Comment from db."

deletePostDraftRecord :: Monad m => Handle m -> PostId -> m ()
deletePostDraftRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostDraft
        [colIdPostPostDraft]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Draft from db."

newPost :: Monad m => Handle m -> [SqlValue] -> m (Either Text Post)
newPost handle [idPost, title, created_at, text] = do
  let postId = fromSql idPost
      postTitle = fromSql title
      createdAt = fromSql created_at
      body = fromSql text
  photoMainE <- getPostMainPhotoRecords handle postId
  photosAddE <- getPostAddPhotoRecords handle postId
  commentsE <- getPostCommentRecords handle postId
  tagsE <- runEitherT $ do 
    tagIds <- EitherT $ getPostTagRecords handle postId
    tags <- EitherT $ DBT.getTagRecordsByIds handle tagIds
    return tags
  let photoMainM = rightToMaybe photoMainE
      photosAddM = rightToMaybe photosAddE
      commentsM = rightToMaybe commentsE
      tagsM = rightToMaybe tagsE
  runEitherT $ do
    authorId <- EitherT $ getPostAuthorRecord handle postId
    author <- EitherT $ DBA.getAuthorRecord handle authorId
    catId <- EitherT $ getPostCategoryRecord handle postId
    cat <- EitherT $ DBC.getCatRecordByCatId handle catId
    return Post {
      post_id = postId,
      post_title = postTitle,
      post_createdAt = createdAt,
      post_text = body,
      post_mainPhoto = photoMainM,
      post_addPhotos = photosAddM,
      post_comments = commentsM,
      post_tags = tagsM,
      post_author = author,
      post_category = cat
    }
newPost _ _ = return $ Left "Invalid Post!"