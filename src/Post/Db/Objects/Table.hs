module Post.Db.Objects.Table where

import Data.Text (Text)

import qualified Post.Db.Objects.Column as DbColumn

data Table = Table
  { name :: Text,
    columns :: [DbColumn.Column]
  }
  deriving (Show)

{- Tables:
** Table: migrations (table contains info about every migration):
    - number - number of migration;
    - description - description of migration;
** Table: users (table contains info every User):
    - id - unique identifier for this User;
    - is_admin - True, if this user is a Admin;
    - first_name - first name of the User;
    - last_name - last name of the User;
    - login - User's login;
    - password - User's password;
    - token - User's current token;
** Table: authors (table contains info every Author):
    - id - unique identifier for User;
    - description - Author's description;
** Table: categories (table contains info every Category):
    - id - unique identifier for this Category;
    - title - title of the Category;
    - subcategory_id - id of subcategory;
** Table: tags (table contains info every Tag):
    - id - unique identifier for this Tag;
    - title - title of the Tag;
** Table: posts (table contains info every Post):
    - id - unique identifier for this Post;
    - title - title of the Post;
    - created_at - date of Post creation;
    - text - text of the Post;
** Table: comments (table contains info every Comment):
    - id - unique identifier for this Comment;
    - text - text of the Comment;
    - post_id - unique identifier of the corresponding Post (one-to-one);
    - user_id - unique identifier of the corresponding User (many-to-one);
** Table: drafts (table contains info every Draft):
    - id - unique identifier for this Draft;
    - text - text of the Draft;
** Table: photos (table contains info every Photo):
    - id - unique identifier for this Photo;
    - link - link of the Photo;
**** Relations
** Table: user_photo (defines one-to-one relation between user and photo):
    - user_id - User id;
    - photo_id - Photo id;
** Table: author_user (defines one-to-one relation between author and user):
    - author_id - Author id;
    - user_id - User id;
** Table: post_author (defines many-to-one relation between post and author):
    - post_id - Post id;
    - author_id - Author id;
** Table: post_category (defines one-to-one relation between post and category):
    - post_id - Post id;
    - category_id - Category id;
** Table: post_tag (defines one-to-many relation between post and tag):
    - post_id - Post id;
    - tag_id - Tag id;
** Table: post_draft (defines one-to-one relation between post and draft):
    - post_id - Post id;
    - draft_id - Draft id;
** Table: post_main_photo (defines one-to-one relation between post and main photo):
    - post_id - Post id;
    - photo_id - Photo id;
** Table: post_add_photo (defines one-to-many relation between post and additional photos):
    - post_id - Post id;
    - photo_id - Photo id;
-}

tableMigrations :: Table
tableMigrations =
  Table
    { name = "migrations",
      columns =
        [ DbColumn.colNumberMigration,
          DbColumn.colDescMigration
        ]
    }

tablePosts :: Table
tablePosts =
  Table
    { name = "posts",
      columns =
        [ DbColumn.colIdPost,
          DbColumn.colTitlePost,
          DbColumn.colCreatedAtPost,
          DbColumn.colTextPost,
          DbColumn.colIdAuthorPost,
          DbColumn.colIdCategoryPost,
          DbColumn.colIdMainPhotoPost
        ]
    }

tableCats :: Table
tableCats =
  Table
    { name = "categories",
      columns =
        [ DbColumn.colIdCat,
          DbColumn.colTitleCat,
          DbColumn.colSubCatCat
        ]
    }

tableTags :: Table
tableTags =
  Table
    { name = "tags",
      columns =
        [ DbColumn.colIdTag,
          DbColumn.colTitleTag
        ]
    }

tableComs :: Table
tableComs =
  Table
    { name = "comments",
      columns =
        [ DbColumn.colIdCom,
          DbColumn.colTextCom,
          DbColumn.colIdPostCom,
          DbColumn.colIdUserCom
        ]
    }

tableDrafts :: Table
tableDrafts =
  Table
    { name = "drafts",
      columns =
        [ DbColumn.colIdDraft,
          DbColumn.colTextDraft,
          DbColumn.colIdPostDraft
        ]
    }

tableUsers :: Table
tableUsers =
  Table
    { name = "users",
      columns =
        [ DbColumn.colIdUser,
          DbColumn.colIsAdminUser,
          DbColumn.colFNUser,
          DbColumn.colLNUser,
          DbColumn.colLoginUser,
          DbColumn.colPassUser,
          DbColumn.colTokenUser,
          DbColumn.colIdPhotoUser
        ]
    }

tableAuthors :: Table
tableAuthors =
  Table
    { name = "authors",
      columns =
        [ DbColumn.colIdAuthor,
          DbColumn.colDescAuthor,
          DbColumn.colIdUserAuthor
        ]
    }

tablePhotos :: Table
tablePhotos =
  Table
    { name = "photos",
      columns =
        [ DbColumn.colIdPhoto,
          DbColumn.colLinkPhoto
        ]
    }

tablePostTag :: Table
tablePostTag =
  Table
    { name = "post_tag",
      columns =
        [ DbColumn.colIdTagPostTag,
          DbColumn.colIdPostPostTag
        ]
    }

tablePostAuthor :: Table
tablePostAuthor =
  Table
    { name = "post_author",
      columns =
        [ DbColumn.colIdAuthorPostAuthor,
          DbColumn.colIdPostPostAuthor
        ]
    }

tablePostCat :: Table
tablePostCat =
  Table
    { name = "post_category",
      columns =
        [ DbColumn.colIdCatPostCat,
          DbColumn.colIdPostPostCat
        ]
    }

tablePostCom :: Table
tablePostCom =
  Table
    { name = "post_comment",
      columns =
        [ DbColumn.colIdPostPostCom,
          DbColumn.colIdComPostCom
        ]
    }

tablePostDraft :: Table
tablePostDraft =
  Table
    { name = "post_draft",
      columns =
        [ DbColumn.colIdPostPostDraft,
          DbColumn.colIdDraftPostDraft
        ]
    }

tablePostMainPhoto :: Table
tablePostMainPhoto =
  Table
    { name = "post_main_photo",
      columns =
        [ DbColumn.colIdPostPostMainPhoto,
          DbColumn.colIdPhotoPostMainPhoto
        ]
    }

tablePostAddPhoto :: Table
tablePostAddPhoto =
  Table
    { name = "post_add_photo",
      columns =
        [ DbColumn.colIdPostPostAddPhoto,
          DbColumn.colIdPhotoPostAddPhoto
        ]
    }

tableAuthorUser :: Table
tableAuthorUser =
  Table
    { name = "author_user",
      columns =
        [ DbColumn.colIdAuthorAuthorUser,
          DbColumn.colIdUserAuthorUser
        ]
    }

tableUserPhoto :: Table
tableUserPhoto =
  Table
    { name = "user_photo",
      columns =
        [ DbColumn.colIdUserUserPhoto,
          DbColumn.colIdPhotoUserPhoto
        ]
    }

tableUserCom :: Table
tableUserCom =
  Table
    { name = "comment_user",
      columns =
        [ DbColumn.colIdUserUserCom,
          DbColumn.colIdComUserCom
        ]
    }
