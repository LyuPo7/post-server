{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.Account where

import qualified Data.ByteString.Char8 as BC
import Control.Monad (when)
import Database.HDBC (IConnection, handleSql, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.PostgreSQL
import Data.Text (Text, pack)
import Crypto.Scrypt (encryptPassIO, defaultParams, getEncryptedPass, Pass(..), EncryptedPass(..), verifyPass)

import qualified Post.Logger as PL
import qualified Post.LoggerIO as PLIO
import qualified Post.Server.Util as Util
import qualified Post.Server.Objects as PSO
import qualified Post.DB.Post as DBP

-- | DB methods for Account
getToken :: IConnection conn => conn -> PL.Handle -> String -> String -> IO (Maybe PSO.Token, String)
getToken dbh logh login password =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT password FROM users WHERE login = ?" [toSql login]
        case r of
            [] -> do
                PL.logWarning logh $ "Incorrect login: " ++ login
                return (Nothing, "Incorrect login: " ++ login)
            [[passwordDB]] -> do
                let encrypted = EncryptedPass {getEncryptedPass = BC.pack (fromSql passwordDB :: String)}
                let (res, _) = verifyPass defaultParams (Pass $ BC.pack password) encrypted
                (if res 
                    then ( do 
                      token <- Util.createToken
                      _ <- run dbh "UPDATE users SET token = ? WHERE login = ?" [toSql token, toSql login]
                      commit dbh
                      PL.logWarning logh $ "User with login: " ++ login ++ " entered."
                      return (Just $ newToken token, "Successfully entered!"))
                    else return (Nothing, "Incorrect password!"))
    where errorHandler e = do fail $ "Error: Error in getToken!\n" ++ show e
          newToken token = PSO.Token {PSO.token = pack token}

checkAdminPerm :: IConnection conn => conn -> PL.Handle -> String -> IO PSO.Permission
checkAdminPerm dbh logh token =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT is_admin FROM users WHERE token = ?" [toSql token]
        case r of
            [] -> do
                PL.logWarning logh $ "Incorrect token: " ++ token
                return PSO.NoPerm
            [[isAdmin]] -> do
                PL.logInfo logh "Authentication is successfull."
                (if (fromSql isAdmin :: Bool)
                    then return PSO.AdminPerm
                    else return PSO.NoPerm)
    where errorHandler e = do fail $ "Error: Error in checkAdminPerm!\n" ++ show e

checkUserPerm :: IConnection conn => conn -> PL.Handle -> String -> IO PSO.Permission
checkUserPerm dbh logh token =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM users WHERE token = ?" [toSql token]
        case r of
            [] -> do
                PL.logWarning logh $ "Incorrect token: " ++ token
                return PSO.NoPerm
            _ -> do
                PL.logInfo logh "Authentication is successfull."
                return PSO.UserPerm
    where errorHandler e = do fail $ "Error: Error in checkUserPerm!\n" ++ show e

checkAuthorWritePerm :: IConnection conn => conn -> PL.Handle -> String -> IO PSO.Permission
checkAuthorWritePerm dbh logh token = do
    authorIdMaybe <- getAuthorId dbh logh token
    case authorIdMaybe of
        Nothing -> do
            PL.logError logh "This User isn't Author."
            return PSO.NoPerm
        Just _ -> do
            PL.logInfo logh "Given access for Post creation."
            return PSO.AuthorWritePerm
    where errorHandler e = do fail $ "Error: Error in checkAuthorWritePerm!\n" ++ show e

checkAuthorReadPerm :: IConnection conn => conn -> PL.Handle -> String -> Integer -> IO PSO.Permission
checkAuthorReadPerm dbh logh token postId = do
    authorPostIdMaybe <- DBP.getPostAuthorId dbh logh postId
    case authorPostIdMaybe of
        Nothing -> do
            PL.logError logh "No exists dependency between Post and Author."
            return PSO.NoPerm
        Just authorPostId -> do
            authorIdMaybe <- getAuthorId dbh logh token
            case authorIdMaybe of
                Nothing -> do
                    PL.logWarning logh $ "Incorrect token: " ++ token
                    return PSO.NoPerm
                Just authorId -> do
                    PL.logInfo logh "Authentication is successfull."
                    let action | authorId == authorPostId = return PSO.AuthorReadPerm
                               | otherwise = return PSO.NoPerm
                    action 
    where errorHandler e = do fail $ "Error: Error in checkAuthorReadPerm!\n" ++ show e

getUserId :: IConnection conn => conn -> PL.Handle -> String -> IO (Maybe Integer)
getUserId dbh logh token =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT id FROM users WHERE token = ?" [toSql token]
        case r of
            [] -> do
                PL.logWarning logh $ "Incorrect token: " ++ token
                return Nothing
            [[id]] -> do
                PL.logInfo logh "Getting User Id corresponding token."
                return $ Just (fromSql id :: Integer)
    where errorHandler e = do fail $ "Error: Error in getUserId!\n" ++ show e

getAuthorId :: IConnection conn => conn -> PL.Handle -> String -> IO (Maybe Integer)
getAuthorId dbh logh token =
    handleSql errorHandler $ do
        authorIdMaybe <- getUserId dbh logh token
        case authorIdMaybe of
            Nothing -> do
                PL.logWarning logh "No user corresponding token"
                return Nothing
            Just userId -> do
                PL.logInfo logh "Getting Author Id corresponding token."
                r <- quickQuery' dbh "SELECT author_id FROM author_user WHERE user_id = ?" [toSql userId]
                case r of
                    [] -> do
                        PL.logWarning logh "No author corresponding token"
                        return Nothing
                    [[authorId]] -> do
                        PL.logInfo logh "Getting Author Id corresponding token."
                        return $ Just (fromSql authorId :: Integer)
    where errorHandler e = do fail $ "Error: Error in getAuthorId!\n" ++ show e