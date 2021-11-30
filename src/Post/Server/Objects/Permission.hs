module Post.Server.Objects.Permission where

import qualified Post.Server.Objects.Synonyms as ServerSynonyms

data Permission = AdminPerm
                | AuthorReadPerm  -- Access to create Posts.
                | AuthorWritePerm ServerSynonyms.AuthorId -- Access to get/publish Drafts
                                  -- and set Photos for Post.
                | UserPerm ServerSynonyms.UserId
                | NoPerm
                deriving (Show, Eq, Ord)