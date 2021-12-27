module Post.Db.Post where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Convertible.Base (convert)
import Data.Either.Combinators (rightToMaybe)
import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..))
import Database.HDBC (SqlValue, fromSql, toSql)

import qualified Post.Db.Author as DbAuthor
import qualified Post.Db.Category as DbCategory
import qualified Post.Db.Comment as DbComment
import qualified Post.Db.DbQuery as DbQuery
import qualified Post.Db.Objects.Column as DbColumn
import qualified Post.Db.Objects.Synonyms as DbSynonyms
import qualified Post.Db.Objects.Table as DbTable
import qualified Post.Db.Photo as DbPhoto
import qualified Post.Db.Tag as DbTag
import qualified Post.Logger as Logger
import qualified Post.Server.Objects.Comment as ServerComment
import qualified Post.Server.Objects.Photo as ServerPhoto
import qualified Post.Server.Objects.Post as ServerPost
import qualified Post.Server.Objects.Synonyms as ServerSynonyms
import qualified Post.Server.ServerSpec as ServerSpec
import qualified Post.Server.Util as ServerUtil

createPost ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  Text ->
  ServerSynonyms.AuthorId ->
  ServerSynonyms.CategoryId ->
  [ServerSynonyms.TagId] ->
  m (Either Text ServerSynonyms.PostId)
createPost handle title text authorId catId tagIds = do
  let logH = ServerSpec.hLogger handle
  postIdE <- getPostIdByTitle handle title
  case postIdE of
    Left _ -> runEitherT $ do
      _ <- newEitherT $ DbCategory.getCatRecordByCatId handle catId
      _ <- newEitherT $ DbTag.getTagRecordsByIds handle tagIds
      lift $ insertPostRecord handle title text authorId catId
      postId <- newEitherT $ getLastPostRecord handle
      lift $ mapM_ (createPostTagDep handle postId) tagIds
      return postId
    Right _ -> do
      let msg =
            "Post with title: '"
              <> convert title
              <> "' already exists."
      Logger.logWarning logH msg
      return $ Left msg

getPosts ::
  Monad m =>
  ServerSpec.Handle m ->
  [DbSynonyms.PostQuery] ->
  ServerSynonyms.Offset ->
  m (Either Text [ServerPost.Post])
getPosts handle postQuery offset = do
  let logH = ServerSpec.hLogger handle
  -- Request to Db table posts
  idAllPosts <- DbQuery.searchPost handle postQuery
  idCatPosts <- DbQuery.searchCat handle postQuery
  idTagPosts <- DbQuery.searchTag handle postQuery
  idAuthorPosts <- DbQuery.searchAuthor handle postQuery
  -- Search request to Db table posts
  idSearch <- DbQuery.findIn handle postQuery
  -- Requests
  let idAllSimple =
        ( ( idAllPosts
              `intersect` idCatPosts
          )
            `intersect` idTagPosts
        )
          `intersect` idAuthorPosts
      idAll = idAllSimple `intersect` idSearch
  -- Request with sorting
  case idAll of
    [] -> do
      Logger.logWarning logH "No Posts in db!"
      return $ Left "No Posts!"
    correctIds -> do
      Logger.logInfo logH "Sorting Posts from db!"
      postsSql <-
        DbQuery.sortQuery
          handle
          postQuery
          (map toSql correctIds)
          offset
      case postsSql of
        [] -> do
          Logger.logWarning logH "No Posts in db!"
          return $ Left "No Posts!"
        ids -> do
          posts <- mapM (getPostRecord handle) ids
          return $ sequence posts

removePost ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerSynonyms.PostId)
removePost handle postId = runEitherT $ do
  _ <- newEitherT $ getPostRecord handle postId
  _ <- lift $ removePostTagDep handle postId
  _ <- lift $ removePostAddPhotoDep handle postId
  _ <- lift $ removePostCommentDep handle postId
  _ <- lift $ removePostDraftDep handle postId
  lift $ deletePostRecord handle postId
  return postId

setPostMainPhoto ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  Text ->
  m (Either Text ServerSynonyms.PhotoId)
setPostMainPhoto handle postId path = do
  let logH = ServerSpec.hLogger handle
  photoIdE <- DbPhoto.savePhoto handle path
  case photoIdE of
    Left _ -> do
      let msg =
            "Couldn't set Main Photo for Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logError logH msg
      return $ Left msg
    Right photoId -> do
      oldPhotoIdE <- getPostMainPhotoIdByPostId handle postId
      case oldPhotoIdE of
        Left _ -> updatePostMainPhotoRecord handle postId photoId
        Right _ -> updatePostMainPhotoRecord handle postId photoId

setPostAddPhoto ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  Text ->
  m (Either Text ServerSynonyms.PhotoId)
setPostAddPhoto handle postId path = do
  let logH = ServerSpec.hLogger handle
  photoIdE <- DbPhoto.savePhoto handle path
  case photoIdE of
    Left _ -> do
      let msg =
            "Couldn't set Additional Photo for Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logError logH msg
      return $ Left msg
    Right photoId -> insertPostAddPhotoRecord handle postId photoId

createPostTagDep ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  ServerSynonyms.TagId ->
  m (Either Text ServerSynonyms.TagId)
createPostTagDep handle postId tagId = do
  let logH = ServerSpec.hLogger handle
  tagsIdE <- getPostTagIdsByPostId handle postId
  case tagsIdE of
    Left _ -> insertPostTagRecord handle postId tagId
    Right tags -> do
      if tagId `elem` tags
        then do
          let msg =
                "Dependency between \
                \Post and Tag already exists."
          Logger.logWarning logH msg
          return $ Left msg
        else do
          Logger.logInfo logH "Inserting dependency between Post and Tag in db."
          insertPostTagRecord handle postId tagId

removePostTagDep ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text [ServerSynonyms.TagId])
removePostTagDep handle postId = runEitherT $ do
  tagsId <- newEitherT $ getPostTagIdsByPostId handle postId
  lift $ deletePostTagRecord handle postId
  return tagsId

removePostAddPhotoDep ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text [ServerSynonyms.PhotoId])
removePostAddPhotoDep handle postId = runEitherT $ do
  photosAdd <- newEitherT $ getPostAddPhotoIdsByPostId handle postId
  lift $ deletePostAddPhotoRecords handle postId
  return $ map ServerPhoto.id photosAdd

removePostCommentDep ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text [ServerSynonyms.CommentId])
removePostCommentDep handle postId = runEitherT $ do
  comments <- newEitherT $ getPostCommentRecords handle postId
  lift $ deletePostComRecords handle postId
  return $ map ServerComment.id comments

removePostDraftDep ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerSynonyms.DraftId)
removePostDraftDep handle postId = runEitherT $ do
  draftId <- newEitherT $ getPostDraftIdByPostId handle postId
  lift $ deletePostDraftRecord handle postId
  return draftId

getPostRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerPost.Post)
getPostRecord handle postId = do
  let logH = ServerSpec.hLogger handle
  postSQL <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePosts
      [ DbColumn.colIdPost,
        DbColumn.colTitlePost,
        DbColumn.colCreatedAtPost,
        DbColumn.colTextPost
      ]
      [DbColumn.colIdPost]
      [toSql postId]
  case postSQL of
    [] -> do
      let msg =
            "No exists Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg
    [post] -> do
      Logger.logInfo logH "Getting Post from db."
      newPost handle post
    _ -> do
      let msg =
            "Violation of Unique Post record in db: \
            \exist more than one record for Post with Id: "
              <> ServerUtil.convertValue postId
      Logger.logError logH msg
      return $ Left msg

getLastPostRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  m (Either Text ServerSynonyms.PostId)
getLastPostRecord handle = do
  let logH = ServerSpec.hLogger handle
  idPostSql <-
    DbQuery.selectFromOrderLimit
      handle
      DbTable.tablePosts
      [DbColumn.colIdPost]
      DbColumn.colIdPost
      1
  case idPostSql of
    [] -> do
      let msg = "No exist Posts!"
      Logger.logWarning logH msg
      return $ Left msg
    [[idPost]] -> do
      let postId = fromSql idPost
      Logger.logInfo logH $
        "Last Post inserted in db with id: "
          <> ServerUtil.convertValue postId
      return $ Right postId
    _ -> do
      let msg = "Incorrect Post record!"
      Logger.logWarning logH msg
      return $ Left msg

getPostIdByTitle ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  m (Either Text ServerSynonyms.PostId)
getPostIdByTitle handle title = do
  let logH = ServerSpec.hLogger handle
  postIdSQL <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePosts
      [DbColumn.colIdPost]
      [DbColumn.colTitlePost]
      [toSql title]
  case postIdSQL of
    [] -> do
      let msg =
            "No exists Post with title: '"
              <> convert title
              <> "'!"
      Logger.logError logH msg
      return $ Left msg
    [[idPost]] -> do
      Logger.logInfo logH $
        "Getting PostId corresponding to title: '"
          <> convert title
          <> "' from db."
      return $ Right $ fromSql idPost
    _ -> do
      let msg =
            "Violation of Unique record Post in db: \
            \exist more than one record for Post with title: '"
              <> convert title
              <> "'!"
      Logger.logWarning logH msg
      return $ Left msg

getPostAuthorIdByPostId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerSynonyms.AuthorId)
getPostAuthorIdByPostId handle postId = do
  let logH = ServerSpec.hLogger handle
  authorIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePosts
      [DbColumn.colIdAuthorPost]
      [DbColumn.colIdPost]
      [toSql postId]
  case authorIdSql of
    [] -> do
      let msg =
            "No exists Author corresponding to Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logInfo logH msg
      return $ Left msg
    [[authorId]] -> do
      Logger.logInfo logH $
        "Getting AuthorId \
        \corresponding to Post with id: "
          <> ServerUtil.convertValue postId
          <> " from db."
      return $ Right $ fromSql authorId
    _ -> do
      let msg =
            "Violation of Unique record Post-Author in db: \
            \exist more than one record for Post with Id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg

getPostCategoryIdByPostId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerSynonyms.CategoryId)
getPostCategoryIdByPostId handle postId = do
  let logH = ServerSpec.hLogger handle
  catIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePosts
      [DbColumn.colIdCategoryPost]
      [DbColumn.colIdPost]
      [toSql postId]
  case catIdSql of
    [] -> do
      let msg =
            "No exists Category corresponding to Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logInfo logH msg
      return $ Left msg
    [[catId]] -> do
      Logger.logInfo logH $
        "Getting CategoryId \
        \corresponding to Post with id: "
          <> ServerUtil.convertValue postId
          <> " from db."
      return $ Right $ fromSql catId
    _ -> do
      let msg =
            "Violation of Unique record Post-Category in db: \
            \exist more than one record for Post with Id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg

getPostTagIdsByPostId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text [ServerSynonyms.TagId])
getPostTagIdsByPostId handle postId = do
  let logH = ServerSpec.hLogger handle
  tagsIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePostTag
      [DbColumn.colIdTagPostTag]
      [DbColumn.colIdPostPostTag]
      [toSql postId]
  case tagsIdSql of
    [] -> do
      let msg =
            "No exist Tags corresponding to Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logInfo logH msg
      return $ Left msg
    tagIds -> do
      Logger.logInfo logH $
        "Getting TagId \
        \corresponding to Post with id: "
          <> ServerUtil.convertValue postId
          <> " from db."
      return $ Right $ map fromSql $ concat tagIds

getPostMainPhotoIdByPostId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerPhoto.Photo)
getPostMainPhotoIdByPostId handle postId = do
  let logH = ServerSpec.hLogger handle
  photoIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePosts
      [DbColumn.colIdMainPhotoPost]
      [DbColumn.colIdPost]
      [toSql postId]
  case photoIdSql of
    [] -> do
      let msg =
            "No exists Main Photo for Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg
    [[photoIdM]] -> do
      case fromSql photoIdM :: Maybe ServerSynonyms.PhotoId of
        Just photoId -> do
          Logger.logInfo logH $
            "Getting Main Photo for Post with id: "
              <> ServerUtil.convertValue postId
              <> "."
          DbPhoto.getPhotoRecordById handle photoId
        Nothing -> do
          let msg =
                "User with id: "
                  <> ServerUtil.convertValue postId
                  <> " hasn't photo."
          Logger.logInfo logH msg
          return $ Left msg
    _ -> do
      let msg =
            "Violation of Unique record Post-MainPhoto in db: \
            \exist more than one record for Post with Id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg

getPostAddPhotoIdsByPostId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text [ServerPhoto.Photo])
getPostAddPhotoIdsByPostId handle postId = do
  let logH = ServerSpec.hLogger handle
  photoIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tablePostAddPhoto
      [DbColumn.colIdPhotoPostAddPhoto]
      [DbColumn.colIdPostPostAddPhoto]
      [toSql postId]
  case photoIdSql of
    [] -> do
      let msg =
            "No exist Add Photos for Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg
    ids -> do
      Logger.logInfo logH $
        "Getting Add Photos for Post with id: "
          <> ServerUtil.convertValue postId
          <> " from db."
      let addPhotoIds = map fromSql $ concat ids
      addPhotos <- mapM (DbPhoto.getPhotoRecordById handle) addPhotoIds
      return $ sequence addPhotos

getPostDraftIdByPostId ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text ServerSynonyms.DraftId)
getPostDraftIdByPostId handle postId = do
  let logH = ServerSpec.hLogger handle
  draftIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableDrafts
      [DbColumn.colIdDraft]
      [DbColumn.colIdPostDraft]
      [toSql postId]
  case draftIdSql of
    [] -> do
      let msg =
            "No exists Draft corresponding to Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logError logH msg
      return $ Left msg
    [[draftId]] -> do
      Logger.logInfo logH "Dependency between Post and Draft already exists."
      return $ Right $ fromSql draftId
    _ -> do
      let msg =
            "Violation of Unique record Post-Draft in db: \
            \exist more than one record for Post with Id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg

getPostDraftIdsByPostIds ::
  Monad m =>
  ServerSpec.Handle m ->
  [ServerSynonyms.PostId] ->
  m (Either Text [ServerSynonyms.DraftId])
getPostDraftIdsByPostIds handle postIds = do
  let logH = ServerSpec.hLogger handle
  draftIdsSql <-
    DbQuery.selectFromWhereIn
      handle
      DbTable.tableDrafts
      [DbColumn.colIdDraft]
      DbColumn.colIdPostDraft
      $ map toSql postIds
  case draftIdsSql of
    [] -> do
      let msg =
            "No exists Drafts corresponding to Posts with id: "
              <> T.intercalate "," (map ServerUtil.convertValue postIds)
      Logger.logError logH msg
      return $ Left msg
    draftIds -> do
      Logger.logInfo logH $
        "Getting Drafts of Posts with Id: "
          <> T.intercalate "," (map ServerUtil.convertValue postIds)
      return $ Right $ map fromSql $ concat draftIds

getPostCommentRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m (Either Text [ServerComment.Comment])
getPostCommentRecords handle postId = do
  let logH = ServerSpec.hLogger handle
  comsIdSql <-
    DbQuery.selectFromWhere
      handle
      DbTable.tableComs
      [DbColumn.colIdCom]
      [DbColumn.colIdPostCom]
      [toSql postId]
  case comsIdSql of
    [] -> do
      let msg =
            "No exist Comments for Post with id: "
              <> ServerUtil.convertValue postId
      Logger.logWarning logH msg
      return $ Left msg
    ids -> do
      Logger.logInfo logH $
        "Getting Comments for Post with id: "
          <> ServerUtil.convertValue postId
          <> " from db."
      let commentIds = map fromSql $ concat ids
      comments <- mapM (DbComment.getCommentRecord handle) commentIds
      return $ sequence comments

insertPostRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.Title ->
  Text ->
  ServerSynonyms.AuthorId ->
  ServerSynonyms.CategoryId ->
  m ()
insertPostRecord handle title text authorId catId = do
  let logH = ServerSpec.hLogger handle
  time <- ServerSpec.getCurrentTime handle
  let day = utctDay time
  DbQuery.insertIntoValues
    handle
    DbTable.tablePosts
    [DbColumn.colTitlePost, DbColumn.colTextPost, DbColumn.colCreatedAtPost, DbColumn.colIdAuthorPost, DbColumn.colIdCategoryPost]
    [toSql title, toSql text, toSql day, toSql authorId, toSql catId]
  Logger.logInfo logH $
    "Post with title: '"
      <> convert title
      <> "' was successfully inserted in db."

updatePostMainPhotoRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  ServerSynonyms.PhotoId ->
  m (Either Text ServerSynonyms.PhotoId)
updatePostMainPhotoRecord handle postId photoId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.updateSetWhere
    handle
    DbTable.tablePosts
    [DbColumn.colIdMainPhotoPost]
    [DbColumn.colIdPost]
    [toSql photoId]
    [toSql postId]
  Logger.logInfo logH "Post's Main Photo was successfully set."
  return $ Right photoId

insertPostAddPhotoRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  ServerSynonyms.PhotoId ->
  m (Either Text ServerSynonyms.PhotoId)
insertPostAddPhotoRecord handle postId photoId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.insertIntoValues
    handle
    DbTable.tablePostAddPhoto
    [DbColumn.colIdPhotoPostAddPhoto, DbColumn.colIdPostPostAddPhoto]
    [toSql photoId, toSql postId]
  Logger.logInfo logH "Post's Add Photo was successfully inserted in db."
  return $ Right photoId

insertPostTagRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  ServerSynonyms.TagId ->
  m (Either Text ServerSynonyms.TagId)
insertPostTagRecord handle postId tagId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.insertIntoValues
    handle
    DbTable.tablePostTag
    [DbColumn.colIdTagPostTag, DbColumn.colIdPostPostTag]
    [toSql tagId, toSql postId]
  Logger.logInfo logH "Creating dependency between Post and Tag."
  return $ Right tagId

deletePostRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m ()
deletePostRecord handle postId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tablePosts
    [DbColumn.colIdPost]
    [toSql postId]
  Logger.logInfo logH $
    "Removing Post with id: "
      <> ServerUtil.convertValue postId
      <> " from db."

deletePostTagRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m ()
deletePostTagRecord handle postId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tablePostTag
    [DbColumn.colIdPostPostTag]
    [toSql postId]
  Logger.logInfo logH "Removing dependency between Post and Tag."

deletePostAddPhotoRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m ()
deletePostAddPhotoRecords handle postId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tablePostAddPhoto
    [DbColumn.colIdPostPostAddPhoto]
    [toSql postId]
  Logger.logInfo
    logH
    "Removing dependency between \
    \Post and Additional Photo from db."

deletePostComRecords ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m ()
deletePostComRecords handle postId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tableComs
    [DbColumn.colIdPostCom]
    [toSql postId]
  Logger.logInfo logH "Removing dependency between Post and Comment from db."

deletePostDraftRecord ::
  Monad m =>
  ServerSpec.Handle m ->
  ServerSynonyms.PostId ->
  m ()
deletePostDraftRecord handle postId = do
  let logH = ServerSpec.hLogger handle
  DbQuery.deleteWhere
    handle
    DbTable.tableDrafts
    [DbColumn.colIdPostDraft]
    [toSql postId]
  Logger.logInfo logH "Removing dependency between Post and Draft from db."

newPost ::
  Monad m =>
  ServerSpec.Handle m ->
  [SqlValue] ->
  m (Either Text ServerPost.Post)
newPost handle [idPost, title, created_at, text] = do
  let postId = fromSql idPost
      postTitle = fromSql title
      createdAt = fromSql created_at
      body = fromSql text
  photoMainE <- getPostMainPhotoIdByPostId handle postId
  photosAddE <- getPostAddPhotoIdsByPostId handle postId
  commentsE <- getPostCommentRecords handle postId
  tagsE <- runEitherT $ do
    tagIds <- newEitherT $ getPostTagIdsByPostId handle postId
    newEitherT $ DbTag.getTagRecordsByIds handle tagIds
  let photoMainM = rightToMaybe photoMainE
      photosAddM = rightToMaybe photosAddE
      commentsM = rightToMaybe commentsE
      tagsM = rightToMaybe tagsE
  runEitherT $ do
    authorId <- newEitherT $ getPostAuthorIdByPostId handle postId
    author <- newEitherT $ DbAuthor.getAuthorRecord handle authorId
    catId <- newEitherT $ getPostCategoryIdByPostId handle postId
    cat <- newEitherT $ DbCategory.getCatRecordByCatId handle catId
    return
      ServerPost.Post
        { ServerPost.id = postId,
          ServerPost.title = postTitle,
          ServerPost.createdAt = createdAt,
          ServerPost.text = body,
          ServerPost.mainPhoto = photoMainM,
          ServerPost.addPhotos = photosAddM,
          ServerPost.comments = commentsM,
          ServerPost.tags = tagsM,
          ServerPost.author = author,
          ServerPost.category = cat
        }
newPost _ _ = return $ Left "Invalid Post!"
