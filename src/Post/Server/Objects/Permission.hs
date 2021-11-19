module Post.Server.Objects.Permission where

data Permission = AdminPerm
                | AuthorReadPerm  -- Access to create Posts.
                | AuthorWritePerm -- Access to get/publish Drafts a
                                  -- and set Photos for Post.
                | UserPerm
                | NoPerm
                deriving (Show, Eq, Ord)