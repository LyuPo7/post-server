{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Post where

import qualified Data.Text as T
import Database.HDBC (SqlValue, fromSql, toSql)
import Data.Time.Clock (UTCTime(..))
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

{-- | DB methods for Post --}
{-- | Create new Post and all Post-Dependencies
      if doesn't already exist Post with the same Title --}
createPost :: Monad m => Handle m -> Title ->
              Text -> AuthorId -> CategoryId -> [TagId] -> m (Either Text PostId)
createPost handle title text authorId catId tagIds = do
  let logh = hLogger handle
  postIdE <- getPostIdByTitle handle title
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
            <> "' already exists."
      Logger.logWarning logh msg
      return $ Left msg

-- | Get all Post records corresponding [PostQuery]
getPosts :: Monad m => Handle m -> [PostQuery] -> Offset -> m (Either Text [Post])
getPosts handle postQuery offset = do
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
      postsSql <- sortQuery handle postQuery (map toSql correctIds) offset
      case postsSql of
        [] -> do
          Logger.logWarning logh "No Posts in db!"
          return $ Left "No Posts!"
        ids -> do
          posts <- mapM (getPostRecord handle) ids
          return $ sequence posts

-- | Remove Post record and all Post-Dependecies
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

{-- Set Main Post Photo 
    if Main Post Photo doesn't exist - create
    if Main Post Photo exists - update --}
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
      oldPhotoIdE <- getPostMainPhotoIdByPostId handle postId
      case oldPhotoIdE of
        Left _ -> insertPostMainPhotoRecord handle postId photoId
        Right _ -> updatePostMainPhotoRecord handle postId photoId

-- | Add Additional Post Photo
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

-- | Create Post-Author record if doesn't exist 
createPostAuthorDep :: Monad m => Handle m ->
                       PostId -> AuthorId -> m (Either Text AuthorId)
createPostAuthorDep handle postId authorId = do
  let logh = hLogger handle
  postAuthorDepE <- getPostAuthorIdbyPostId handle postId
  case postAuthorDepE of
    Left _ -> do
      _ <- insertPostAuthorRecord handle postId authorId
      return $ Right authorId
    Right _ -> do
      let msg = "Dependency between Post and Author already exists."
      Logger.logError logh msg 
      return $ Left msg

-- | Create Post-Category record if doesn't exist 
createPostCatDep :: Monad m => Handle m ->
                    PostId -> CategoryId -> m (Either Text CategoryId)
createPostCatDep handle postId catId = do
  let logh = hLogger handle
  postCatDepE <- getPostCategoryIdByPostId handle postId
  case postCatDepE of
    Left _ -> do
      _ <- insertPostCatRecord handle postId catId
      return $ Right catId
    Right _ -> do
      let msg = "Dependency between \
                \Post and Category already exists."
      Logger.logError logh msg
      return $ Left msg

-- | Create Post-Tag record if doesn't exist 
createPostTagDep :: Monad m => Handle m -> PostId -> TagId -> m (Either Text TagId)
createPostTagDep handle postId tagId = do
  let logh = hLogger handle
  tagsIdE <- getPostTagIdsByPostId handle postId
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

-- | Create Post-Draft record if doesn't exist 
createPostDraftDep :: Monad m => Handle m ->
                      PostId -> DraftId -> m (Either Text DraftId)
createPostDraftDep handle postId draftId = do
  let logh = hLogger handle
  draftIdDbE <- getPostDraftIdByPostId handle postId
  case draftIdDbE of
    Left _ -> insertPostDraftRecord handle postId draftId
    Right _ -> do
      let msg = "Draft for Post with id: "
            <> convert postId
            <> " already exists!"
      Logger.logError logh msg
      return $ Left msg

-- | Remove Post-Author record if exists
removePostAuthorDep :: Monad m => Handle m -> PostId -> m (Either Text AuthorId)
removePostAuthorDep handle postId = runEitherT $ do
  authorId <- EitherT $ getPostAuthorIdbyPostId handle postId
  lift $ deletePostAuthorRecord handle postId
  return authorId

-- | Remove Post-Category record if exists
removePostCatDep :: Monad m => Handle m -> PostId -> m (Either Text CategoryId)
removePostCatDep handle postId = runEitherT $ do
  catId <- EitherT $ getPostCategoryIdByPostId handle postId
  lift $ deletePostCatRecord handle postId
  return catId

-- | Remove Post-Tag records if exist
removePostTagDep :: Monad m => Handle m -> PostId -> m (Either Text [TagId])
removePostTagDep handle postId = runEitherT $ do
  tagsId <- EitherT $ getPostTagIdsByPostId handle postId
  lift $ deletePostTagRecord handle postId
  return tagsId

-- | Remove Post-Main-Photo record if exists
removePostMainPhotoDep :: Monad m => Handle m -> PostId -> m (Either Text PhotoId)
removePostMainPhotoDep handle postId = runEitherT $ do
  photoId <- EitherT $ getPostMainPhotoIdByPostId handle postId
  lift $ deletePostMainPhotoRecord handle postId
  return $ photo_id photoId

-- | Remove Post-Additional-Photo records if exist
removePostAddPhotoDep :: Monad m => Handle m -> PostId -> m (Either Text [PhotoId])
removePostAddPhotoDep handle postId = runEitherT $ do
  photosAdd <- EitherT $ getPostAddPhotoIdsByPostId handle postId
  lift $ deletePostAddPhotoRecords handle postId
  return $ map photo_id photosAdd

-- | Remove Post-Main-Comment records if exist
removePostCommentDep :: Monad m => Handle m -> PostId -> m (Either Text [PhotoId])
removePostCommentDep handle postId = runEitherT $ do
  comments <- EitherT $ getPostCommentRecords handle postId
  lift $ deletePostComRecords handle postId
  return $ map comment_id comments    

-- | Remove Post-Draft record if exists
removePostDraftDep :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
removePostDraftDep handle postId = runEitherT $ do
  draftId <- EitherT $ getPostDraftIdByPostId handle postId
  lift $ deletePostDraftRecord handle postId
  return draftId

-- | Get Post record by PostId if exists
getPostRecord :: Monad m => Handle m -> PostId -> m (Either Text Post)
getPostRecord handle postId = do
  let logh = hLogger handle
  postSQL <- selectFromWhere handle tablePosts
              [colIdPost, colTitlePost, colCreatedAtPost, colTextPost]
              [colIdPost]
              [toSql postId]
  case postSQL of
    [] -> do
      let msg = "No exists Post with id: "
            <> convert postId
      Logger.logWarning logh msg
      return $ Left msg
    [post] -> do
      Logger.logInfo logh "Getting Post from db."
      newPost handle post
    _ -> do
      let msg = "Violation of Unique Post record in db: \
                \exist more than one record for Post with Id: "
                  <> convert postId
      Logger.logError logh msg
      return $ Left msg

-- | RemGet Last Post record if exists
getLastPostRecord :: Monad m => Handle m -> m (Either Text PostId)
getLastPostRecord handle = do
  let logh = hLogger handle
  idPostSql <- selectFromOrderLimit handle tablePosts
                [colIdPost]
                 colIdPost 1
  case idPostSql of
    [] -> do
      let msg = "No exist Posts!"
      Logger.logWarning logh msg
      return $ Left msg
    [[idPost]] -> do
      let postId = fromSql idPost
      Logger.logInfo logh $ "Last Post inserted in db with id: "
        <> convert postId
      return $ Right postId
    _ -> do
      let msg = "Incorrect Post record!"
      Logger.logWarning logh msg
      return $ Left msg

-- | Get PostId by Title if exists
getPostIdByTitle :: Monad m => Handle m -> Title -> m (Either Text PostId)
getPostIdByTitle handle title = do
  let logh = hLogger handle
  postIdSQL <- selectFromWhere handle tablePosts
                [colIdPost]
                [colTitlePost]
                [toSql title]
  case postIdSQL of
    [] -> do
      let msg = "No exists Post with title: '"
            <> title
            <> "'!"
      Logger.logError logh msg
      return $ Left msg
    [[idPost]] -> do
      Logger.logInfo logh $ "Getting PostId corresponding to title: '"
        <> title
        <> "' from db."
      return $ Right $ fromSql idPost
    _ -> do
      let msg = "Violation of Unique record Post in db: \
                \exist more than one record for Post with title: '"
                  <> title
                  <> "'!"
      Logger.logWarning logh msg
      return $ Left msg

-- | Get AuthorId by PostId if exists
getPostAuthorIdbyPostId :: Monad m => Handle m -> PostId -> m (Either Text AuthorId)
getPostAuthorIdbyPostId handle postId = do
  let logh = hLogger handle
  authorIdSql <- selectFromWhere handle tablePostAuthor
                  [colIdAuthorPostAuthor]
                  [colIdPostPostAuthor]
                  [toSql postId]
  case authorIdSql of
    [] -> do
      let msg = "No exists Author corresponding to Post with id: "
            <> convert postId
      Logger.logInfo logh msg
      return $ Left msg
    [[authorId]] -> do
      Logger.logInfo logh $ "Getting AuthorId \
           \corresponding to Post with id: "
        <> convert postId
        <> " from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg = "Violation of Unique record Post-Author in db: \
                \exist more than one record for Post with Id: "
                  <> convert postId
      Logger.logWarning logh msg
      return $ Left msg
    
-- | Get CategoryId by PostId if exists
getPostCategoryIdByPostId :: Monad m => Handle m -> 
                             PostId -> m (Either Text CategoryId)
getPostCategoryIdByPostId handle postId = do
  let logh = hLogger handle
  catIdSql <- selectFromWhere handle tablePostCat
               [colIdCatPostCat]
               [colIdPostPostCat]
               [toSql postId]
  case catIdSql of
    [] -> do
      let msg = "No exists Category corresponding to Post with id: "
            <> convert postId
      Logger.logInfo logh msg
      return $ Left msg
    [[catId]] -> do
      Logger.logInfo logh $ "Getting CategoryId \
           \corresponding to Post with id: "
        <> convert postId
        <> " from db."
      return $ Right $ fromSql catId
    _ -> do
      let msg = "Violation of Unique record Post-Category in db: \
                \exist more than one record for Post with Id: "
                  <> convert postId
      Logger.logWarning logh msg
      return $ Left msg

-- | Get [TagId] by PostId if exist
getPostTagIdsByPostId :: Monad m => Handle m -> PostId -> m (Either Text [TagId])
getPostTagIdsByPostId handle postId = do
  let logh = hLogger handle
  tagsIdSql <- selectFromWhere handle tablePostTag
                [colIdTagPostTag]
                [colIdPostPostTag]
                [toSql postId]
  case tagsIdSql of
    [] -> do
      let msg = "No exist Tags corresponding to Post with id: "
            <> convert postId
      Logger.logInfo logh msg
      return $ Left msg
    tagIds -> do
      Logger.logInfo logh $ "Getting TagId \
           \corresponding to Post with id: "
        <> convert postId
        <> " from db."
      return $ Right $ map fromSql $ concat tagIds

-- | Get Main-PhotoId by PostId if exists
getPostMainPhotoIdByPostId :: Monad m => Handle m -> PostId -> m (Either Text Photo)
getPostMainPhotoIdByPostId handle postId = do
  let logh = hLogger handle
  photoIdSql <- selectFromWhere handle tablePostMainPhoto
                 [colIdPhotoPostMainPhoto]
                 [colIdPostPostMainPhoto]
                 [toSql postId]
  case photoIdSql of
    [] -> do
      let msg = "No exists Main Photo for Post with id: "
            <> convert postId
      Logger.logWarning logh msg
      return $ Left msg
    [[photoId]] -> do
      Logger.logInfo logh $ "Getting Main Photo for Post with id: "
        <> convert postId <> "."
      DBPh.getPhotoRecordById handle $ fromSql photoId
    _ -> do
      let msg = "Violation of Unique record Post-MainPhoto in db: \
                \exist more than one record for Post with Id: "
                  <> convert postId
      Logger.logWarning logh msg
      return $ Left msg

-- | Get Additional-[PhotoId] by PostId if exist
getPostAddPhotoIdsByPostId :: Monad m => Handle m -> PostId -> m (Either Text [Photo])
getPostAddPhotoIdsByPostId handle postId = do
  let logh = hLogger handle
  photoIdSql <- selectFromWhere handle tablePostAddPhoto
                 [colIdPhotoPostAddPhoto]
                 [colIdPostPostAddPhoto]
                 [toSql postId]
  case photoIdSql of
    [] -> do
      let msg = "No exist Add Photos for Post with id: "
            <> convert postId
      Logger.logWarning logh msg
      return $ Left msg
    ids -> do
      Logger.logInfo logh $ "Getting Add Photos for Post with id: "
        <> convert postId
        <> " from db."
      let addPhotoIds = map fromSql $ concat ids
      addPhotos <- mapM (DBPh.getPhotoRecordById handle) addPhotoIds
      return $ sequence addPhotos

-- | Get DraftId by PostId if exists
getPostDraftIdByPostId :: Monad m => Handle m -> PostId -> m (Either Text DraftId)
getPostDraftIdByPostId handle postId = do
  let logh = hLogger handle
  draftIdSql <- selectFromWhere handle tablePostDraft
                 [colIdDraftPostDraft]
                 [colIdPostPostDraft]
                 [toSql postId]
  case draftIdSql of
    [] -> do
      let msg = "No exists Draft corresponding to Post with id: "
            <> convert postId
      Logger.logError logh msg
      return $ Left msg
    [[draftId]] -> do 
      Logger.logInfo logh "Dependency between Post and Draft already exists."
      return $ Right $ fromSql draftId
    _ -> do
      let msg = "Violation of Unique record Post-Draft in db: \
                \exist more than one record for Post with Id: "
                  <> convert postId
      Logger.logWarning logh msg
      return $ Left msg

-- | Get all [DraftId] by [PostId] if exist
getPostDraftIdsByPostIds :: Monad m => Handle m ->
                           [PostId] -> m (Either Text [DraftId])
getPostDraftIdsByPostIds handle postIds = do
  let logh = hLogger handle
  draftIdsSql <- selectFromWhereIn handle tablePostDraft
                 [colIdDraftPostDraft]
                  colIdPostPostDraft
                  $ map toSql postIds
  case draftIdsSql of
    [] -> do
      let msg = "No exists Drafts corresponding to Posts with id: "
            <> T.intercalate "," (map convert postIds)
      Logger.logError logh msg
      return $ Left msg
    draftIds -> do 
      Logger.logInfo logh $ "Getting Drafts of Posts with Id: "
        <> T.intercalate "," (map convert postIds)
      return $ Right $ map fromSql $ concat draftIds

-- | Get all Comment records by PostId
getPostCommentRecords :: Monad m => Handle m ->
                         PostId -> m (Either Text [Comment])
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
      Logger.logWarning logh msg
      return $ Left msg
    ids -> do
      Logger.logInfo logh $ "Getting Comments for Post with id: "
        <> convert postId
        <> " from db."
      let commentIds = map fromSql $ concat ids
      comments <- mapM (DBCo.getCommentRecord handle) commentIds
      return $ sequence comments

-- | Insert Post record
insertPostRecord :: Monad m => Handle m -> Title -> Text -> m ()
insertPostRecord handle title text = do
  let logh = hLogger handle
  time <- getCurrentTime handle
  let day = utctDay time
  _ <- insertIntoValues handle tablePosts
        [colTitlePost, colTextPost, colCreatedAtPost]
        [toSql title, toSql text, toSql day]
  Logger.logInfo logh $ "Post with title: '"
    <> title
    <> "' was successfully inserted in db."

-- | Update Post-Main-Photo record
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

-- | Insert Post-Main-Photo record
insertPostMainPhotoRecord :: Monad m => Handle m ->
                             PostId -> PhotoId -> m (Either Text PhotoId)
insertPostMainPhotoRecord handle postId photoId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostMainPhoto
        [colIdPhotoPostMainPhoto, colIdPostPostMainPhoto] 
        [toSql photoId, toSql postId]
  Logger.logInfo logh "Post's Main Photo was successfully updated."
  return $ Right photoId

-- | Insert Post-Additional-Photo record
insertPostAddPhotoRecord :: Monad m => Handle m ->
                            PostId -> PhotoId -> m (Either Text PhotoId)
insertPostAddPhotoRecord handle postId photoId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostAddPhoto
        [colIdPhotoPostAddPhoto, colIdPostPostAddPhoto] 
        [toSql photoId, toSql postId]
  Logger.logInfo logh "Post's Add Photo was successfully inserted in db."
  return $ Right photoId

-- | Insert Post-Author record
insertPostAuthorRecord :: Monad m => Handle m -> PostId -> AuthorId -> m ()
insertPostAuthorRecord handle postId authorId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostAuthor 
        [colIdPostPostAuthor, colIdAuthorPostAuthor] 
        [toSql postId, toSql authorId]
  Logger.logInfo logh "Creating dependency between Post and Author."

-- | Insert Post-Category record
insertPostCatRecord :: Monad m => Handle m -> PostId -> CategoryId -> m ()
insertPostCatRecord handle postId catId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostCat 
        [colIdPostPostCat, colIdCatPostCat] 
        [toSql postId, toSql catId]
  Logger.logInfo logh "Creating dependency between Post and Category."

-- | Insert Post-Tag record
insertPostTagRecord :: Monad m => Handle m -> PostId -> TagId -> m (Either Text TagId)
insertPostTagRecord handle postId tagId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostTag
        [colIdTagPostTag, colIdPostPostTag] 
        [toSql tagId, toSql postId]
  Logger.logInfo logh "Creating dependency between Post and Tag."
  return $ Right tagId

-- | Insert Post-Draft record
insertPostDraftRecord :: Monad m => Handle m ->
                         PostId -> DraftId -> m (Either Text DraftId)
insertPostDraftRecord handle postId draftId = do
  let logh = hLogger handle
  _ <- insertIntoValues handle tablePostDraft
        [colIdDraftPostDraft, colIdPostPostDraft] 
        [toSql draftId, toSql postId]
  Logger.logInfo logh "Creating dependency between Post and Draft."
  return $ Right draftId

-- | Delete Post record
deletePostRecord :: Monad m => Handle m -> PostId -> m ()
deletePostRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePosts
        [colIdPost]
        [toSql postId]
  Logger.logInfo logh $ "Removing Post with id: "
    <> convert postId
    <> " from db."

-- | Delete Post-Author record
deletePostAuthorRecord :: Monad m => Handle m -> PostId -> m ()
deletePostAuthorRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostAuthor
        [colIdPostPostAuthor]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Author."

-- | Delete Post-Category record
deletePostCatRecord :: Monad m => Handle m -> PostId -> m ()
deletePostCatRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostCat
        [colIdPostPostCat]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Category."

-- | Delete Post-Tag record
deletePostTagRecord :: Monad m => Handle m -> PostId -> m ()
deletePostTagRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostTag
        [colIdPostPostTag]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Tag."

-- | Delete Post-Main-Photo record
deletePostMainPhotoRecord :: Monad m => Handle m -> PostId -> m ()
deletePostMainPhotoRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostMainPhoto
        [colIdPostPostMainPhoto]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Main Photo from db."

-- | Delete Post-Additional-Photo record
deletePostAddPhotoRecords :: Monad m => Handle m -> PostId -> m ()
deletePostAddPhotoRecords handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostAddPhoto
        [colIdPostPostAddPhoto]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between \
                      \Post and Additional Photo from db."

-- | Delete Post-Comment record
deletePostComRecords :: Monad m => Handle m -> PostId -> m ()
deletePostComRecords handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostCom
        [colIdPostPostCom]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Comment from db."

-- | Delete Post-Draft record
deletePostDraftRecord :: Monad m => Handle m -> PostId -> m ()
deletePostDraftRecord handle postId = do
  let logh = hLogger handle
  _ <- deleteWhere handle tablePostDraft
        [colIdPostPostDraft]
        [toSql postId]
  Logger.logInfo logh "Removing dependency between Post and Draft from db."

-- | Create Post from [SqlValue]
newPost :: Monad m => Handle m -> [SqlValue] -> m (Either Text Post)
newPost handle [idPost, title, created_at, text] = do
  let postId = fromSql idPost
      postTitle = fromSql title
      createdAt = fromSql created_at
      body = fromSql text
  photoMainE <- getPostMainPhotoIdByPostId handle postId
  photosAddE <- getPostAddPhotoIdsByPostId handle postId
  commentsE <- getPostCommentRecords handle postId
  tagsE <- runEitherT $ do 
    tagIds <- EitherT $ getPostTagIdsByPostId handle postId
    EitherT $ DBT.getTagRecordsByIds handle tagIds
  let photoMainM = rightToMaybe photoMainE
      photosAddM = rightToMaybe photosAddE
      commentsM = rightToMaybe commentsE
      tagsM = rightToMaybe tagsE
  runEitherT $ do
    authorId <- EitherT $ getPostAuthorIdbyPostId handle postId
    author <- EitherT $ DBA.getAuthorRecord handle authorId
    catId <- EitherT $ getPostCategoryIdByPostId handle postId
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