{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Post.Server.Objects.Synonyms where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Convertible.Base (ConvertError (..), Convertible (..), convert)
import Data.Text (Text)
import Database.HDBC (SqlValue (..))

newtype UserId = UserId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible UserId SqlValue where
  safeConvert (UserId updateId) = safeConvert updateId

instance Convertible SqlValue UserId where
  safeConvert (SqlInteger updateId) = Right $ UserId updateId
  safeConvert (SqlInt64 updateId) = Right $ UserId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ UserId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "UserId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'UserId'"
        }

newtype AuthorId = AuthorId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible AuthorId SqlValue where
  safeConvert (AuthorId updateId) = safeConvert updateId

instance Convertible SqlValue AuthorId where
  safeConvert (SqlInteger updateId) = Right $ AuthorId updateId
  safeConvert (SqlInt64 updateId) = Right $ AuthorId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ AuthorId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "AuthorId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'AuthorId'"
        }

newtype CategoryId = CategoryId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible CategoryId SqlValue where
  safeConvert (CategoryId updateId) = safeConvert updateId

instance Convertible SqlValue CategoryId where
  safeConvert (SqlInteger updateId) = Right $ CategoryId updateId
  safeConvert (SqlInt64 updateId) = Right $ CategoryId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ CategoryId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "CategoryId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'CategoryId'"
        }

newtype TagId = TagId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible TagId SqlValue where
  safeConvert (TagId updateId) = safeConvert updateId

instance Convertible SqlValue TagId where
  safeConvert (SqlInteger updateId) = Right $ TagId updateId
  safeConvert (SqlInt64 updateId) = Right $ TagId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ TagId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "TagId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'TagId'"
        }

newtype CommentId = CommentId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible CommentId SqlValue where
  safeConvert (CommentId updateId) = safeConvert updateId

instance Convertible SqlValue CommentId where
  safeConvert (SqlInteger updateId) = Right $ CommentId updateId
  safeConvert (SqlInt64 updateId) = Right $ CommentId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ CommentId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "CommentId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'CommentId'"
        }

newtype DraftId = DraftId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible DraftId SqlValue where
  safeConvert (DraftId updateId) = safeConvert updateId

instance Convertible SqlValue DraftId where
  safeConvert (SqlInteger updateId) = Right $ DraftId updateId
  safeConvert (SqlInt64 updateId) = Right $ DraftId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ DraftId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "DraftId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'DraftId'"
        }

newtype PostId = PostId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible PostId SqlValue where
  safeConvert (PostId updateId) = safeConvert updateId

instance Convertible SqlValue PostId where
  safeConvert (SqlInteger updateId) = Right $ PostId updateId
  safeConvert (SqlInt64 updateId) = Right $ PostId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ PostId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "PostId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'PostId'"
        }

newtype PhotoId = PhotoId Integer
  deriving newtype (Show, Num, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible PhotoId SqlValue where
  safeConvert (PhotoId updateId) = safeConvert updateId

instance Convertible SqlValue PhotoId where
  safeConvert (SqlInteger updateId) = Right $ PhotoId updateId
  safeConvert (SqlInt64 updateId) = Right $ PhotoId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ PhotoId $ toInteger updateId
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "PhotoId",
          convErrorMessage = "No Integer/Int64/Int32 Value in field 'PhotoId'"
        }

newtype Password = Password Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible Password Text where
  safeConvert (Password pass) = Right pass

instance Convertible Text Password where
  safeConvert pass = Right $ Password pass

instance Convertible Password SqlValue where
  safeConvert (Password pass) = safeConvert pass

instance Convertible SqlValue Password where
  safeConvert (SqlString pass) = Right $ Password $ convert pass
  safeConvert (SqlByteString pass) = Right $ Password $ convert pass
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "Password",
          convErrorMessage = "No SqlString Value in field 'Password'"
        }

newtype Login = Login Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible Login Text where
  safeConvert (Login pass) = Right pass

instance Convertible Text Login where
  safeConvert pass = Right $ Login pass

instance Convertible Login SqlValue where
  safeConvert (Login pass) = safeConvert pass

instance Convertible SqlValue Login where
  safeConvert (SqlString login) = Right $ Login $ convert login
  safeConvert (SqlByteString login) = Right $ Login $ convert login
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "Login",
          convErrorMessage = "No SqlString Value in field 'Login'"
        }

newtype FirstName = FirstName Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible FirstName Text where
  safeConvert (FirstName pass) = Right pass

instance Convertible Text FirstName where
  safeConvert pass = Right $ FirstName pass

instance Convertible FirstName SqlValue where
  safeConvert (FirstName pass) = safeConvert pass

instance Convertible SqlValue FirstName where
  safeConvert (SqlString fn) = Right $ FirstName $ convert fn
  safeConvert (SqlByteString fn) = Right $ FirstName $ convert fn
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "FirstName",
          convErrorMessage = "No SqlString Value in field 'FirstName'"
        }

newtype LastName = LastName Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible LastName Text where
  safeConvert (LastName pass) = Right pass

instance Convertible Text LastName where
  safeConvert pass = Right $ LastName pass

instance Convertible LastName SqlValue where
  safeConvert (LastName pass) = safeConvert pass

instance Convertible SqlValue LastName where
  safeConvert (SqlString ln) = Right $ LastName $ convert ln
  safeConvert (SqlByteString ln) = Right $ LastName $ convert ln
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "LastName",
          convErrorMessage = "No SqlString Value in field 'LastName'"
        }

newtype Description = Description Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible Description Text where
  safeConvert (Description pass) = Right pass

instance Convertible Text Description where
  safeConvert pass = Right $ Description pass

instance Convertible Description SqlValue where
  safeConvert (Description pass) = safeConvert pass

instance Convertible SqlValue Description where
  safeConvert (SqlString d) = Right $ Description $ convert d
  safeConvert (SqlByteString d) = Right $ Description $ convert d
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "Description",
          convErrorMessage = "No SqlString Value in field 'Description'"
        }

newtype Title = Title Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible Title Text where
  safeConvert (Title pass) = Right pass

instance Convertible Text Title where
  safeConvert pass = Right $ Title pass

instance Convertible Title SqlValue where
  safeConvert (Title pass) = safeConvert pass

instance Convertible SqlValue Title where
  safeConvert (SqlString t) = Right $ Title $ convert t
  safeConvert (SqlByteString t) = Right $ Title $ convert t
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "Title",
          convErrorMessage = "No SqlString Value in field 'Title'"
        }

newtype Link = Link Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible Link Text where
  safeConvert (Link pass) = Right pass

instance Convertible Text Link where
  safeConvert pass = Right $ Link pass

instance Convertible Link SqlValue where
  safeConvert (Link pass) = safeConvert pass

instance Convertible SqlValue Link where
  safeConvert (SqlString l) = Right $ Link $ convert l
  safeConvert (SqlByteString l) = Right $ Link $ convert l
  safeConvert x =
    Left $
      ConvertError
        { convSourceValue = show x,
          convSourceType = "SqlValue",
          convDestType = "Link",
          convErrorMessage = "No SqlString Value in field 'Link'"
        }

newtype Token = Token Text
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)

instance Convertible Text Token where
  safeConvert pass = Right $ Token pass

instance Convertible Token SqlValue where
  safeConvert (Token pass) = safeConvert pass

newtype Offset = Offset Integer
  deriving newtype (Show, Read, Ord, Eq, FromJSON, ToJSON)
