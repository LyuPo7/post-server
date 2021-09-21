{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Post where

import Control.Monad (when, liftM, join)
import Database.HDBC (IConnection, SqlValue(..), handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text)
import Data.List (intersect, union, intercalate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Maybe (fromMaybe)
--import Data.Time.Clock (UTCTime(..), Day)

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Author as DBA
import qualified Post.DB.Category as DBC
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Photo as DBPh
import qualified Post.DB.Comment as DBCo
import qualified Post.DB.Data as DbData

-- | DB methods for Post
createPost :: IConnection conn => conn -> PL.Handle -> String -> String -> Integer -> Integer -> [Integer] -> IO (Maybe String)
createPost dbh logh title text authorId catId tagIds =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM posts WHERE title = ?" [toSql title]
        case r of
            [] -> do
                time <- getCurrentTime
                --let createdAt = utctDay time
                _ <- run dbh "INSERT INTO posts (title, text, created_at) VALUES (?,?,?)"
                        [toSql title, toSql text, toSql time]
                commit dbh
                PL.logInfo logh $ "Post with title: " ++ title ++ " was successfully inserted in db."
                r <- quickQuery' dbh "SELECT id FROM posts ORDER BY id DESC LIMIT 1" []
                case r of
                    [] -> do
                        PL.logError logh "Error while inserting Post to db."
                        return $ Just "Error while inserting Post to db."
                    [[postId]] -> do
                        createPostAuthorDep dbh logh (fromSql postId :: Integer) authorId
                        createPostCatDep dbh logh (fromSql postId :: Integer) catId
                        mapM_ (createPostTagDep dbh logh (fromSql postId :: Integer)) tagIds
                        --createPostMainPhotoDep dbh logh (fromSql postId :: Integer) tagsId
                        --createPostAddPhotoDep dbh logh (fromSql postId :: Integer) tagsId
                        return Nothing
            _ -> do
                PL.logWarning logh $ "Post with title: " ++ title ++ " already exists in db."
                return $ Just $ "Post with title: " ++ title ++ " already exists."
    where errorHandler e = do fail $ "Error: Error in createPost!\n" ++ show e

getPosts :: IConnection conn => conn -> PL.Handle -> DbData.DbReq -> IO (Maybe [PSO.Post])
getPosts dbh logh dbReq =
    handleSql errorHandler $ do
        -- Request to DB table posts
        let dbPostQuery = "SELECT id FROM posts " ++ fst (DbData.dbPostReq dbReq)
        let dbPostArgs = snd $ DbData.dbPostReq dbReq
        PL.logDebug logh $ "Query to table 'posts': " ++ dbPostQuery
        rPost <- quickQuery' dbh dbPostQuery dbPostArgs
        -- Request to DB table post_category
        let dbCatQuery = "SELECT post_id FROM post_category " ++ fst (DbData.dbCatReq dbReq)
        let dbCatArgs = snd $ DbData.dbCatReq dbReq
        PL.logDebug logh $ "Query to table 'post_category': " ++ dbCatQuery
        rCat <- quickQuery' dbh dbCatQuery dbCatArgs
        -- Request to DB table post_tag
        let dbTagQuery = "SELECT post_id FROM post_tag " ++ fst (DbData.dbTagReq dbReq)
        let dbTagArgs = snd $ DbData.dbTagReq dbReq
        PL.logDebug logh $ "Query to table 'post_tag': " ++ dbTagQuery
        rTag <- quickQuery' dbh dbTagQuery dbTagArgs
        -- Request to DB table post_author
        let dbAuthorQuery = "SELECT post_id FROM post_author " ++ fst (DbData.dbAuthorReq dbReq)
        let dbAuthorArgs = snd $ DbData.dbAuthorReq dbReq
        PL.logDebug logh $ "Query to table 'post_tag': " ++ dbAuthorQuery
        rAuthor <- quickQuery' dbh dbAuthorQuery dbAuthorArgs
        -- Serch request to DB table posts
        let dbPostSearchQuery = "SELECT id FROM posts " ++ fst (DbData.dbPostSearch dbReq)
        let dbPostSearchArgs = snd $ DbData.dbPostSearch dbReq
        PL.logDebug logh $ "Search Query to table 'posts': " ++ dbPostSearchQuery
        rPostSearch <- quickQuery' dbh dbPostSearchQuery dbPostSearchArgs
        -- Serch request to DB table post_author
        let dbAuthorSearchQuery = "SELECT post_id FROM post_author " ++ fst (DbData.dbAuthorSearch dbReq)
        let dbAuthorSearchArgs = snd $ DbData.dbAuthorSearch dbReq
        PL.logDebug logh $ "Query to table 'post_tag': " ++ dbAuthorSearchQuery
        rAuthorSearch <- quickQuery' dbh dbAuthorSearchQuery dbAuthorSearchArgs
        -- Serch request to DB table post_category
        let dbCatSearchQuery = "SELECT post_id FROM post_category " ++ fst (DbData.dbCatSearch dbReq)
        let dbCatSearchArgs = snd $ DbData.dbCatSearch dbReq
        PL.logDebug logh $ "Query to table 'post_category': " ++ dbCatSearchQuery
        rCatSearch <- quickQuery' dbh dbCatSearchQuery dbCatSearchArgs
        -- Serch request to DB table post_tag
        let dbTagSearchQuery = "SELECT post_id FROM post_tag " ++ fst (DbData.dbTagSearch dbReq)
        let dbTagSearchArgs = snd $ DbData.dbTagSearch dbReq
        PL.logDebug logh $ "Query to table 'post_tag': " ++ dbTagSearchQuery
        rTagSearch <- quickQuery' dbh dbTagSearchQuery dbTagSearchArgs
        -- Requests
        let rAllSimple = ((rPost `intersect` rCat) `intersect` rTag) `intersect` rAuthor
        let rAllSearch = ((rPostSearch `union` rCatSearch) `union` rTagSearch) `union` rAuthorSearch
        let rAll = rAllSimple `intersect` rAllSearch
        -- Request with sorting
        case rAll of
            [] -> do
                PL.logWarning logh "No posts in db!"
                return Nothing
            _ -> do
                let idAll = concat rAll
                let nAll = length idAll
                PL.logInfo logh "Sorting posts from db!"
                rMain <- quickQuery' dbh (DbData.orderQuery dbReq ++ intercalate "," (replicate nAll "?") ++ DbData.orderBy dbReq) idAll
                case rAll of
                    [] -> do
                        PL.logWarning logh "No posts in db!"
                        return Nothing
                    ids -> do
                        posts <- mapM (getPost dbh logh . (\ x -> fromSql x :: Integer)) (concat ids)
                        return $ sequence posts
    where errorHandler e = do fail $ "Error: Error in getPosts!\n" ++ show e

getPost :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Post)
getPost dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id, title, created_at, text FROM posts WHERE id = ?" [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "No post with id: " ++ show postId  ++ " in db!"
                return Nothing
            [params@[id, title, created_at, text]] -> do
                authorId <- getPostAuthorId dbh logh postId
                checkAuthor <- DBA.getAuthor dbh logh (fromMaybe (-1) authorId)
                let author = fromMaybe author0 checkAuthor
                catId <- getPostCategoryId dbh logh postId
                checkCat <- DBC.getCat dbh logh (fromMaybe (-1) catId)
                let cat = fromMaybe cat0 checkCat
                PL.logInfo logh "Getting Post from db."
                --tagIds <- getRelatedTagsId dbh logh postId
                --tags <- map (DBT.getTag dbh logh) $ fromMaybe [(-1)] tagIds
                Just <$> newPost params author cat
    where errorHandler e = do fail $ "Error: Error in getPost!\n" ++ show e
          newPost [id, title, created_at, text] author cat = do
              photoMainMaybe <- getPostMainPhoto dbh logh (fromSql id :: Integer)
              photosAddMaybe <- getPostAddPhotos dbh logh (fromSql id :: Integer)
              commentsMaybe <- getPostComments dbh logh (fromSql id :: Integer)
              return PSO.Post {
                PSO.post_title = fromSql title :: Text,
                PSO.post_createdAt = fromSql created_at :: Text,
                PSO.post_category = cat,
                PSO.post_tags = Nothing,
                PSO.post_text = fromSql text :: Text,
                PSO.post_mainPhoto = photoMainMaybe,
                PSO.post_addPhotos = photosAddMaybe,
                PSO.post_id = fromSql id :: Integer,
                PSO.post_author = author,
                PSO.post_comments = commentsMaybe
              }

removePost :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe String)
removePost dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM posts WHERE user_id = ?" [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists Post with id: " ++ show postId ++  "!"
                return $ Just $ "No exists Post with id: " ++ show postId ++  "!"
            _ -> do
                _ <- run dbh "DELETE FROM posts WHERE id = ?" [toSql postId]
                removePostAuthorDep dbh logh postId
                removePostCatDep dbh logh postId
                removePostTagDep dbh logh postId
                --removePostMainPhotoDep dbh logh postId
                --removePostAddPhotoDep dbh logh postId
                --removePostCommentDep dbh logh postId
                --removePostDraftDep dbh logh postId
                commit dbh
                PL.logInfo logh $ "Removing Post with id: " ++ show postId ++ " from db."
                return Nothing
    where errorHandler e = do fail $ "Error: Error in removePost!\n" ++ show e

setPostMainPhoto :: IConnection conn => conn -> PL.Handle -> Integer -> String -> IO (Maybe String)
setPostMainPhoto dbh logh postId path =
    handleSql errorHandler $ do
        photoIdMaybe <- DBPh.savePhoto dbh logh path
        case photoIdMaybe of
            Nothing -> do
                PL.logError logh $ "Couldn't set Main Photo for Post with id: " ++ show postId
                return $ Just $ "Couldn't set Main Photo for Post with id: " ++ show postId
            Just photoId -> do
                r <- quickQuery' dbh "SELECT photo_id FROM post_main_photo WHERE post_id = ?" 
                  [toSql postId]
                case r of
                    [] -> do
                        _ <- run dbh "INSERT INTO post_main_photo (photo_id, post_id) VALUES (?,?)" 
                             [toSql photoId, toSql postId]
                        commit dbh
                        PL.logInfo logh "Post's Main Photo was successfully set."
                        return Nothing
                    _ -> do
                        PL.logError logh $ "Main Photo for Post with id: " ++ show postId ++ " already exists in db."
                        return $ Just $ "Main Photo for Post with id: " ++ show postId ++ " already exists."
    where errorHandler e = do fail $ "Error: Error in setPostMainPhoto!\n" ++ show e

setPostAddPhoto :: IConnection conn => conn -> PL.Handle -> Integer -> String -> IO (Maybe String)
setPostAddPhoto dbh logh postId path =
    handleSql errorHandler $ do
        photoIdMaybe <- DBPh.savePhoto dbh logh path
        case photoIdMaybe of
            Nothing -> do
                PL.logError logh $ "Couldn't set Additional Photo for Post with id: " ++ show postId
                return $ Just $ "Couldn't set Additional Photo for Post with id: " ++ show postId
            Just photoId -> do
                r <- quickQuery' dbh "SELECT photo_id FROM post_add_photo WHERE post_id = ? AND photo_id = ?" 
                  [toSql postId, toSql photoId]
                case r of
                    [] -> do
                        _ <- run dbh "INSERT INTO post_add_photo (photo_id, post_id) VALUES (?,?)" 
                             [toSql photoId, toSql postId]
                        commit dbh
                        PL.logInfo logh "Post's Add Photo was successfully set."
                        return Nothing
                    _ -> do
                        PL.logError logh $ "Add Photo with id: " ++ show photoId ++ " for Post with id: " ++ show postId ++ " already exists in db."
                        return $ Just $ "Main Photo with id: " ++ show photoId ++ " for Post with id: " ++ show postId ++ " already exists."
    where errorHandler e = do fail $ "Error: Error in setPostAddPhoto!\n" ++ show e

getPostMainPhoto :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe PSO.Photo)
getPostMainPhoto dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT photo_id FROM post_main_photo WHERE post_id = ?" 
            [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists Main Photo for Post with id: " ++ show postId ++ " in db!"
                return Nothing
            [[photoId]] -> do
                PL.logInfo logh $ "Getting Main Photo for Post with id: " ++ show postId ++ "."
                DBPh.getPhoto dbh logh (fromSql photoId :: Integer)
    where errorHandler e = do fail $ "Error: Error in getPostMainPhoto!\n" ++ show e

getPostAddPhotos :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe [PSO.Photo])
getPostAddPhotos dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT photo_id FROM post_add_photo WHERE post_id = ?" 
            [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists Add Photos for Post with id: " ++ show postId ++ " in db!"
                return Nothing
            ids -> do
                PL.logInfo logh $ "Getting Add Photos for Post with id: " ++ show postId ++ "."
                let addPhotoIds = map (\x -> fromSql x :: Integer) $ concat ids
                addPhotos <- mapM (DBPh.getPhoto dbh logh) addPhotoIds
                return $ sequence addPhotos
    where errorHandler e = do fail $ "Error: Error in getPostAddPhotos!\n" ++ show e

getPostComments :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe [PSO.Comment])
getPostComments dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT comment_id FROM post_comment WHERE post_id = ?" 
            [toSql postId]
        case r of
            [] -> do
                PL.logWarning logh $ "No exists Comments for Post with id: " ++ show postId ++ " in db!"
                return Nothing
            ids -> do
                PL.logInfo logh $ "Getting Comments for Post with id: " ++ show postId ++ "."
                let commentIds = map (\x -> fromSql x :: Integer) $ concat ids
                comments <- mapM (DBCo.getComment dbh logh) commentIds
                return $ sequence comments
    where errorHandler e = do fail $ "Error: Error in getPostAddPhotos!\n" ++ show e

getPostAuthorId :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe Integer)
getPostAuthorId dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT author_id FROM post_author WHERE post_id = ?" [toSql postId]
        case r of
            [[x]] -> do
                PL.logInfo logh "Getting author_id corresponding to this Post from db."
                return $ Just $ fromSql x
            _ -> do
                PL.logError logh "No Author corresponding to this Post in db!"
                return Nothing
    where errorHandler e = do fail $ "Error: Error in getPostAuthorId!\n" ++ show e

getPostCategoryId :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe Integer)
getPostCategoryId dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT category_id FROM post_category WHERE post_id = ?" [toSql postId]
        case r of
            [[x]] -> do
                PL.logInfo logh "Getting category_id corresponding to this Post from db."
                return $ Just $ fromSql x
            _ -> do
                PL.logError logh "No Category corresponding to this Post in db!"
                return Nothing
    where errorHandler e = do fail $ "Error: Error in getPostCategoryId!\n" ++ show e

getPostTagsIds :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe [Integer])
getPostTagsIds dbh logh postId =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT user_id FROM post_tags WHERE post_id = ?" [toSql postId]
        case r of
            [xs] -> do
                PL.logInfo logh "Getting tag_id corresponding to this Post from db."
                return $ Just $ map fromSql xs
            _ -> do
                PL.logError logh "No Tags corresponding to this Post in db!"
                return Nothing
    where errorHandler e = do fail $ "Error: Error in getPostTagsIds!\n" ++ show e

getPostDraftId :: IConnection conn => conn -> PL.Handle -> Integer -> IO (Maybe Integer, String)
getPostDraftId dbh logh postId = do
    r <- quickQuery' dbh "SELECT draft_id FROM post_draft WHERE post_id = ?" 
                  [toSql postId]
    case r of
        [] -> do
            PL.logInfo logh "Dependency between Post and Draft doesn't exist."
            return (Nothing, "Dependency between Post and Draft already exists.")
        [[draftId]] -> do 
            PL.logError logh "Dependency between Post and Draft already exists."
            return (Just (fromSql draftId :: Integer), "Dependency between Post and Draft already exists.")
    where errorHandler e = do fail $ "Error: Error in getPostDraftId!\n" ++ show e

createPostAuthorDep :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> IO ()
createPostAuthorDep dbh logh postId authorId = do
    r <- quickQuery' dbh "SELECT author_id FROM post_author WHERE post_id = ?" 
                  [toSql postId]
    case r of
        [] -> do
            _ <- run dbh "INSERT INTO post_author (post_id, author_id) VALUES (?,?)" 
                [toSql postId, toSql authorId]
            commit dbh
            PL.logInfo logh "Creating dependency between Post and Author."
        _ -> do PL.logError logh "Dependency between Post and Author already exists."
    where errorHandler e = do fail $ "Error: Error in createPostAuthorDep!\n" ++ show e

createPostCatDep :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> IO ()
createPostCatDep dbh logh postId catId = do
    r <- quickQuery' dbh "SELECT category_id FROM post_category WHERE post_id = ?" 
                  [toSql postId]
    case r of
        [] -> do
            _ <- run dbh "INSERT INTO post_category (post_id, category_id) VALUES (?,?)" 
                [toSql postId, toSql catId]
            commit dbh
            PL.logInfo logh "Creating dependency between Post and Category."
        _ -> do PL.logError logh "Dependency between Post and Category already exists."
    where errorHandler e = do fail $ "Error: Error in createPostCatDep!\n" ++ show e

createPostTagDep :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> IO ()
createPostTagDep dbh logh postId tagId = do
    r <- quickQuery' dbh "SELECT tag_id FROM post_tag WHERE post_id = ? AND tag_id = ?" 
                  [toSql postId, toSql tagId]
    case r of
        [] -> do
            _ <- run dbh "INSERT INTO post_tag (post_id, tag_id) VALUES (?,?)" 
                [toSql postId, toSql tagId]
            commit dbh
            PL.logInfo logh "Creating dependency between Post and Tag."
        _ -> do PL.logError logh "Dependency between Post and Tag already exists."
    where errorHandler e = do fail $ "Error: Error in createPostTagDep!\n" ++ show e

createPostDraftDep :: IConnection conn => conn -> PL.Handle -> Integer -> Integer -> IO ()
createPostDraftDep dbh logh postId draftId = do
    (draftIdDbMaybe, msg) <- getPostDraftId dbh logh postId
    case draftIdDbMaybe of
        Nothing -> do
            _ <- run dbh "INSERT INTO post_draft (post_id, draft_id) VALUES (?,?)" 
                [toSql postId, toSql draftId]
            commit dbh
            PL.logInfo logh "Creating dependency between Post and Draft."
        Just _ -> do PL.logError logh msg
    where errorHandler e = do fail $ "Error: Error in createPostDraftDep!\n" ++ show e

removePostAuthorDep :: IConnection conn => conn -> PL.Handle -> Integer -> IO ()
removePostAuthorDep dbh logh postId = do
    r <- quickQuery' dbh "SELECT author_id FROM post_author WHERE post_id = ?" 
                  [toSql postId]
    case r of
        [] -> do PL.logError logh "Dependency between Post and Author doesn't exist."
        _ -> do
            _ <- run dbh "DELETE FROM post_author WHERE post_id = ?" [toSql postId]
            commit dbh
            PL.logInfo logh "Removing dependency between Post and Author."
    where errorHandler e = do fail $ "Error: Error in removePostAuthorDep!\n" ++ show e

removePostCatDep :: IConnection conn => conn -> PL.Handle -> Integer -> IO ()
removePostCatDep dbh logh postId = do
    r <- quickQuery' dbh "SELECT author_id FROM post_category WHERE post_id = ?" 
                  [toSql postId]
    case r of
        [] -> do PL.logError logh "Dependency between Post and Category doesn't exist."
        _ -> do
            _ <- run dbh "DELETE FROM post_category WHERE post_id = ?" [toSql postId]
            commit dbh
            PL.logInfo logh "Removing dependency between Post and Category."
    where errorHandler e = do fail $ "Error: Error in removePostCatDep!\n" ++ show e

removePostTagDep :: IConnection conn => conn -> PL.Handle -> Integer -> IO ()
removePostTagDep dbh logh postId = do
    r <- quickQuery' dbh "SELECT author_id FROM post_tag WHERE post_id = ?" 
                  [toSql postId]
    case r of
        [] -> do PL.logError logh "Dependency between Post and Tag doesn't exist."
        _ -> do
            _ <- run dbh "DELETE FROM post_tag WHERE post_id = ?" [toSql postId]
            commit dbh
            PL.logInfo logh "Removing dependency between Post and Tag."
    where errorHandler e = do fail $ "Error: Error in removePostTagDep!\n" ++ show e

user0 = PSO.User {
  PSO.user_id = 1010, -- Unique identifier for this User.
  PSO.user_isAdmin = False, -- True, if this user is a Admin.
  PSO.user_firstName = "Jhon", -- User's first name.
  PSO.user_lastName = "Thomson", -- User's last name.
  PSO.user_photo = Nothing
}

author0 = PSO.Author {
  PSO.author_user = user0, -- User.
  PSO.author_description = "Adventures" -- Author's description.
}

cat0 = PSO.Category {
  PSO.category_id = 0, -- Unique identifier for this Category.
  PSO.category_title = "Sport", -- Title of Category.
  PSO.category_subcategory = Just cat1
}

cat1 = PSO.Category {
  PSO.category_id = 1, -- Unique identifier for this Category.
  PSO.category_title = "Crossfit", -- Title of Category.
  PSO.category_subcategory = Just cat2
}

cat2 = PSO.Category {
  PSO.category_id = 1, -- Unique identifier for this Category.
  PSO.category_title = "Crossfit Games", -- Title of Category.
  PSO.category_subcategory = Nothing
}

tag0 = PSO.Tag {
  PSO.tag_id = 0,
  PSO.tag_title = "sport"
}

tag1 = PSO.Tag {
  PSO.tag_id = 1,
  PSO.tag_title = "crossfit"
}

post0 = PSO.Post {
  PSO.post_id = 0, -- Unique identifier for this Announcement.
  PSO.post_author = author0 , -- Author of Announcement.
  PSO.post_title = "Crossfit Games 2021", -- Title of Announcement.
  PSO.post_createdAt = "03.08.21", -- Date when the Announcement was created.
  PSO.post_category = cat0, -- Category of Announcement.
  PSO.post_tags = Just [tag0,tag1], -- Array of Tag of Announcement.
  PSO.post_text = "Yesterday was the last day of competitions of the 2021 Crossfit Games", -- Text of Announcement.
  PSO.post_mainPhoto = Nothing, -- Main Photo of Announcement.
  PSO.post_addPhotos = Nothing, -- Array of Additional Photos of Announcement.
  PSO.post_comments = Just [com0, com1] -- Array of Comments of Announcement.
}

com0 = PSO.Comment {
  PSO.comment_id = 2, -- Unique identifier for this Comment.
  PSO.comment_text = "Happy!"
}

com1 = PSO.Comment {
  PSO.comment_id = 3, -- Unique identifier for this Comment.
  PSO.comment_text = "Very Happy!"
}

draft0 = PSO.Draft {
  PSO.draft_id = 0 , -- Unique identifier for this Draft.
  PSO.draft_text = "No text"
}