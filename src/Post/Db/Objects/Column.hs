module Post.Db.Objects.Column where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Post.Db.Objects.ColumnType as DbColumnType
import qualified Post.Db.Objects.Property as DbProperty

data Column = Column
  { name :: Text,
    columnType :: DbColumnType.ColumnType,
    properties :: [DbProperty.Property]
  }

instance Show Column where
  show (Column cName cType cProps) = case cProps of
    [] ->
      T.unpack cName
        ++ " "
        ++ show cType
    xs ->
      T.unpack cName
        ++ " "
        ++ show cType
        ++ " "
        ++ unwords (map show $ sort xs)

colNumberMigration :: Column
colNumberMigration =
  Column
    { name = "number",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colDescMigration :: Column
colDescMigration =
  Column
    { name = "description",
      columnType = DbColumnType.TEXT,
      properties = []
    }

colIdPost :: Column
colIdPost =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colTitlePost :: Column
colTitlePost =
  Column
    { name = "title",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colCreatedAtPost :: Column
colCreatedAtPost =
  Column
    { name = "created_at",
      columnType = DbColumnType.TIMESTAMP,
      properties = []
    }

colTextPost :: Column
colTextPost =
  Column
    { name = "text",
      columnType = DbColumnType.TEXT,
      properties = []
    }

colIdAuthorPost :: Column
colIdAuthorPost =
  Column
    { name = "author_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdCat :: Column
colIdCat =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colTitleCat :: Column
colTitleCat =
  Column
    { name = "title",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colSubCatCat :: Column
colSubCatCat =
  Column
    { name = "subcategory_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdTag :: Column
colIdTag =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colTitleTag :: Column
colTitleTag =
  Column
    { name = "title",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdCom :: Column
colIdCom =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties = [DbProperty.PrimaryKey]
    }

colTextCom :: Column
colTextCom =
  Column
    { name = "text",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdPostCom :: Column
colIdPostCom =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdUserCom :: Column
colIdUserCom =
  Column
    { name = "user_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdDraft :: Column
colIdDraft =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colTextDraft :: Column
colTextDraft =
  Column
    { name = "text",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdPostDraft :: Column
colIdPostDraft =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdUser :: Column
colIdUser =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colIsAdminUser :: Column
colIsAdminUser =
  Column
    { name = "is_admin",
      columnType = DbColumnType.BOOLEAN,
      properties =
        [ DbProperty.NotNull
        ]
    }

colFNUser :: Column
colFNUser =
  Column
    { name = "first_name",
      columnType = DbColumnType.TEXT,
      properties = []
    }

colLNUser :: Column
colLNUser =
  Column
    { name = "last_name",
      columnType = DbColumnType.TEXT,
      properties = []
    }

colPatronymicUser :: Column
colPatronymicUser =
  Column
    { name = "patronymic",
      columnType = DbColumnType.TEXT,
      properties = []
    }

colLoginUser :: Column
colLoginUser =
  Column
    { name = "login",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colPassUser :: Column
colPassUser =
  Column
    { name = "password",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colTokenUser :: Column
colTokenUser =
  Column
    { name = "token",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdAuthor :: Column
colIdAuthor =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colDescAuthor :: Column
colDescAuthor =
  Column
    { name = "description",
      columnType = DbColumnType.TEXT,
      properties = []
    }

colIdUserAuthor :: Column
colIdUserAuthor =
  Column
    { name = "user_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdPhoto :: Column
colIdPhoto =
  Column
    { name = "id",
      columnType = DbColumnType.SERIAL,
      properties =
        [ DbProperty.PrimaryKey
        ]
    }

colLinkPhoto :: Column
colLinkPhoto =
  Column
    { name = "link",
      columnType = DbColumnType.TEXT,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdTagPostTag :: Column
colIdTagPostTag =
  Column
    { name = "tag_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdPostPostTag :: Column
colIdPostPostTag =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdAuthorPostAuthor :: Column
colIdAuthorPostAuthor =
  Column
    { name = "author_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdPostPostAuthor :: Column
colIdPostPostAuthor =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdPostPostCat :: Column
colIdPostPostCat =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdCatPostCat :: Column
colIdCatPostCat =
  Column
    { name = "category_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdPostPostCom :: Column
colIdPostPostCom =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdComPostCom :: Column
colIdComPostCom =
  Column
    { name = "comment_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdPostPostDraft :: Column
colIdPostPostDraft =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdDraftPostDraft :: Column
colIdDraftPostDraft =
  Column
    { name = "draft_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.Unique
        ]
    }

colIdPostPostMainPhoto :: Column
colIdPostPostMainPhoto =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdPhotoPostMainPhoto :: Column
colIdPhotoPostMainPhoto =
  Column
    { name = "photo_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdPostPostAddPhoto :: Column
colIdPostPostAddPhoto =
  Column
    { name = "post_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdPhotoPostAddPhoto :: Column
colIdPhotoPostAddPhoto =
  Column
    { name = "photo_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdAuthorAuthorUser :: Column
colIdAuthorAuthorUser =
  Column
    { name = "author_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdUserAuthorUser :: Column
colIdUserAuthorUser =
  Column
    { name = "user_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdUserUserPhoto :: Column
colIdUserUserPhoto =
  Column
    { name = "user_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }

colIdPhotoUserPhoto :: Column
colIdPhotoUserPhoto =
  Column
    { name = "photo_id",
      columnType = DbColumnType.INTEGER,
      properties = []
    }

colIdUserUserCom :: Column
colIdUserUserCom =
  Column
    { name = "user_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull
        ]
    }

colIdComUserCom :: Column
colIdComUserCom =
  Column
    { name = "comment_id",
      columnType = DbColumnType.INTEGER,
      properties =
        [ DbProperty.NotNull,
          DbProperty.Unique
        ]
    }
