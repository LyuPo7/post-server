{-# LANGUAGE OverloadedStrings #-}

module Post.Server.Util where

import qualified Data.ByteString.Char8 as BC
import qualified Data.UUID.V4 as V4
import qualified Data.Text as T
import Data.Text (Text)
import Data.UUID (UUID(..))
import Data.List (intercalate)
import Network.HTTP.Types (Query)
import Control.Monad (join, liftM)
import Database.HDBC (toSql, SqlValue(..))

import qualified Post.DB.Data as DbData

-- types
type Token = String
type Admin = Text
type Server = Text
type PostQuery = [(String, Maybe String)]

createToken :: IO Token
createToken = fmap show V4.nextRandom

admins :: [Admin]
admins = ["lyupo"]

server :: Server
server = "http://localhost:3000"

allLookups :: Eq a => [(a, Maybe a)] -> [a] -> Maybe [a]
allLookups assoc = mapM (\ k -> join $ lookup k assoc)

extractRequired :: Query -> [BC.ByteString] -> Either Text [Text]
extractRequired params paramNames = do
  case allLookups params paramNames of
    Nothing -> Left "Incorrect request"
    Just res -> Right $ map (T.pack . BC.unpack) res

extractOptional :: Query -> [BC.ByteString] -> [Maybe Text]
extractOptional params = map
    (fmap (T.pack . BC.unpack) . (\ param -> join $ lookup param params))

createOptionalDict :: Query -> [BC.ByteString] -> PostQuery
createOptionalDict params = map
    (\param -> (BC.unpack param, fmap BC.unpack . join $ lookup param params))

createDbRequest :: PostQuery -> DbData.DbReq
createDbRequest queryDict = initialReq {
  DbData.dbPostReq = postReq,
  DbData.dbAuthorReq = authorReq,
  DbData.dbTagReq = tagReq,
  DbData.dbCatReq = catReq,
  DbData.dbPostSearch = postSearch,
  DbData.dbAuthorSearch = authorSearch,
  DbData.dbCatSearch = catSearch,
  DbData.dbTagSearch = tagSearch,
  DbData.orderQuery = orderQuery,
  DbData.orderBy = orderBy
  } where
    initialReq = DbData.initialDbReq
    postReq = queryPostToDb $ getNotNullParams DbData.dbPostReqParams queryDict
    authorReq = queryAuthorToDb $ getNotNullParams DbData.dbAuthorReqParams queryDict
    tagReq = queryTagToDb $ getNotNullParams DbData.dbTagReqParams queryDict
    catReq = queryCatToDb $ getNotNullParams DbData.dbCatReqParams queryDict
    postSearch = searchPostToDb $ getNotNullParams DbData.dbSearchParams queryDict
    authorSearch = searchAuthorToDb $ getNotNullParams DbData.dbSearchParams queryDict
    catSearch = searchCatToDb $ getNotNullParams DbData.dbSearchParams queryDict
    tagSearch = searchTagToDb $ getNotNullParams DbData.dbSearchParams queryDict
    orderQuery = orderQueryToDb $ getNotNullParams DbData.dbOrderParams queryDict
    orderBy = orderByToDb $ getNotNullParams DbData.dbOrderParams queryDict

getNotNullParams :: [String] -> PostQuery -> PostQuery
getNotNullParams filtList = filter (\x -> not (null $ snd x) && fst x `elem` filtList)

queryPostToDb :: [(String, Maybe String)] -> (String, [SqlValue])
queryPostToDb params | not $ null args = (query, paramArgs)
                     | otherwise = ("", [])
                     where 
                       query = "WHERE " ++ intercalate " AND " (map keyPostToDb queryArgs) 
                       paramArgs = map (toSql . snd) args
                       queryArgs = map fst args
                       args = filter (not . null . snd) params

keyPostToDb :: String -> String
keyPostToDb "created_at" = "created_at = ? "
keyPostToDb "created_at__lt" = "created_at < ? "
keyPostToDb "created_at__gt" = "created_at > ? "
keyPostToDb "find_in_title" = "title LIKE ?"
keyPostToDb "find_in_text" = "text LIKE ?"

queryCatToDb :: PostQuery -> DbData.DbQuery
queryCatToDb params | not $ null args = (query, paramArgs)
                    | otherwise = ("", [])
                    where
                      query = "WHERE " ++ intercalate " AND " (map keyCatToDb queryArgs) 
                      paramArgs = map (toSql . snd) args
                      queryArgs = map fst args
                      args = filter (not . null . snd) params

keyCatToDb :: String -> String
keyCatToDb "category" = "category_id = ? "

queryTagToDb :: PostQuery -> DbData.DbQuery
queryTagToDb [] = ("", [])
queryTagToDb [(key, value)] = case value of
  Nothing -> ("", [])
  Just param -> (query, paramArgs) where
    query = "WHERE " ++ keyTagToDb key (length paramArgs)
    paramArgs = map toSql args
    args | key == "tag" = read ("[" ++ param ++ "]") :: [Integer]
         | otherwise = read param :: [Integer]
queryTagToDb _ = error "queryTagToDb function: Too many elements in dictionary!"

keyTagToDb :: String -> Int -> DbData.DbQueryString
keyTagToDb "tag" _ = "tag_id = ?"
keyTagToDb "tag__in" n = "tag_id IN (" ++ intercalate "," (replicate n "?") ++ ")"
keyTagToDb "tag__all" n = intercalate " AND " $ replicate n "tag_id = ?"

queryAuthorToDb :: PostQuery -> DbData.DbQuery
queryAuthorToDb [] = ("", [])
queryAuthorToDb [(key, value)] = case value of
  Nothing -> ("", [])
  Just param -> (query, map toSql $ words param) where
    query = "WHERE author_id = (SELECT author_id FROM author_user WHERE user_id = (SELECT id FROM users WHERE first_name = ? AND last_name = ?));"
queryAuthorToDb _ = error "queryAuthorToDb function: Too many elements in dictionary!"

searchPostToDb :: PostQuery -> DbData.DbQuery
searchPostToDb [] = ("", [])
searchPostToDb [(key, value)] = case value of
  Nothing -> ("", [])
  Just param -> (query, map toSql [value, value]) where
    query = "WHERE text LIKE ? OR title LIKE ? "
searchPostToDb _ = error "searchPostToDb function: Too many elements in dictionary!"

searchAuthorToDb :: PostQuery -> DbData.DbQuery
searchAuthorToDb [] = ("", [])
searchAuthorToDb [(key, value)] = case value of
  Nothing -> ("", [])
  Just param -> (query, map toSql [value, value]) where
    query = "WHERE author_id = (SELECT author_id FROM author_user WHERE user_id = (SELECT id FROM users WHERE first_name = ? OR last_name = ?));"
searchAuthorToDb _ = error "searchAuthorToDb function: Too many elements in dictionary!"

searchCatToDb :: PostQuery -> DbData.DbQuery
searchCatToDb [] = ("", [])
searchCatToDb [(key, value)] = case value of
  Nothing -> ("", [])
  Just param -> (query, map toSql [value]) where
    query = "WHERE category_id IN (SELECT id FROM categories WHERE title LIKE ? )"
searchCatToDb _ = error "searchCatToDb function: Too many elements in dictionary!"

searchTagToDb :: PostQuery -> DbData.DbQuery
searchTagToDb [] = ("", [])
searchTagToDb [(key, value)] = case value of
  Nothing -> ("", [])
  Just param -> (query, map toSql [value]) where
    query = "WHERE tag_id IN (SELECT id FROM tags WHERE title LIKE ? )"
searchTagToDb _ = error "searchTagToDb function: Too many elements in dictionary!"

--"SELECT id FROM posts WHERE id IN (" ++ (intercalate "," $ replicate nAll "?") ++ ")  ORDER BY created_at") idAll
--SELECT id FROM post_category JOIN categories ON post_category.category_id=categories.id WHERE post_id IN (1,2) ORDER by title;
--SELECT posts.id, COUNT(*) as photo_count FROM posts LEFT JOIN post_add_photo ON posts.id=post_add_photo.post_id WHERE posts.id IN (1,2,3) GROUP BY posts.id ORDER BY photo_count DESC;
--SELECT id FROM post_author JOIN categories ON post_category.category_id=categories.id
--SELECT id, first_name, last_name FROM post_author INNER JOIN author_user ON post_author.author_id=author_user.author_id INNER JOIN users ON author_user.user_id=users.id WHERE id IN (1,2,3) ORDER BY last_name, first_name;

orderQueryToDb :: PostQuery -> DbData.DbQueryString
orderQueryToDb [] = "SELECT id FROM posts WHERE id IN ("
orderQueryToDb [(key, value)] = case key of
  "order_by_date" -> "SELECT id FROM posts WHERE id IN ("
  "order_by_category" -> "SELECT id FROM post_category JOIN categories ON post_category.category_id=categories.id WHERE post_id IN ("
  "order_by_photos" -> "SELECT posts.id, COUNT(*) as photo_count FROM posts LEFT JOIN post_add_photo ON posts.id=post_add_photo.post_id WHERE posts.id IN ("
  "order_by_author" -> "SELECT id FROM post_author INNER JOIN author_user ON post_author.author_id=author_user.author_id INNER JOIN users ON author_user.user_id=users.id WHERE id IN ("
orderQueryToDb _ = error "orderQueryToDb function: Too many elements in dictionary!"

orderByToDb :: PostQuery -> DbData.DbQueryString
orderByToDb [] = ") ORDER BY created_at"
orderByToDb [(key, value)] = case key of
  "order_by_date" -> ") ORDER BY created_at"
  "order_by_category" -> ") ORDER by title;"
  "order_by_photos" -> ") GROUP BY posts.id ORDER BY photo_count DESC;"
  "order_by_author" -> ") ORDER BY last_name, first_name;"
orderByToDb _ = error "orderByToDb function: Too many elements in dictionary!"

convert :: Show a => a -> Text
convert = T.pack . show