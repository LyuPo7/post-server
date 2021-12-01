import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

import qualified TestPost.Config as Config
import qualified TestPost.Db.Account as DbAccount
import qualified TestPost.Db.Author as DbAuthor
import qualified TestPost.Db.Category as DbCategory
import qualified TestPost.Db.Comment as DbComment
import qualified TestPost.Db.DbQuery as DbQuery
import qualified TestPost.Db.Draft as DbDraft
import qualified TestPost.Db.Photo as DbPhoto
import qualified TestPost.Db.Post as DbPost
import qualified TestPost.Db.Tag as DbTag
import qualified TestPost.Db.User as DbUser
import qualified TestPost.Server.QueryParameters as Query
import qualified TestPost.Server.Util as Util

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ DbQuery.spec_queryFromWhere,
          DbQuery.spec_queryFromWhereIn,
          DbQuery.spec_queryFromWhereInLimit,
          DbQuery.spec_queryFromOrderLimitOffset,
          DbQuery.spec_queryFromOrderLimit,
          DbQuery.spec_queryDeleteWhere,
          DbQuery.spec_queryInsertIntoValues,
          DbQuery.spec_queryUpdateSetWhere,
          DbQuery.spec_querySpecialPosts,
          DbQuery.spec_querySearchPost,
          DbQuery.spec_querySearchCat,
          DbQuery.spec_querySearchTag,
          DbQuery.spec_querySearchAuthor,
          DbQuery.spec_findInPosts,
          DbQuery.spec_findInAuthors,
          DbQuery.spec_findInCats,
          DbQuery.spec_findInTags,
          DbQuery.spec_querySort,
          DbTag.spec_newTag,
          DbTag.spec_getTagPostRecords,
          DbTag.spec_getTagRecordsById,
          DbTag.spec_getAllTagRecords,
          DbTag.spec_getTagIdByTitle,
          DbUser.spec_newUser,
          DbUser.spec_getAuthorIdByUserId,
          DbUser.spec_getUserRecords,
          DbUser.spec_getUserRecordById,
          DbUser.spec_getUserIdByLogin,
          DbPhoto.spec_newPhoto,
          DbPhoto.spec_getLastPhotoRecord,
          DbPhoto.spec_getPhotoRecordById,
          DbPhoto.spec_getPhotoIdByName,
          DbDraft.spec_newDraft,
          DbDraft.spec_getDraftText,
          DbDraft.spec_getDraftRecords,
          DbComment.spec_newComment,
          DbComment.spec_getLastCommentRecord,
          DbComment.spec_getCommentRecord,
          DbCategory.spec_getSub,
          DbCategory.spec_newCatNull,
          DbCategory.spec_newCat,
          DbCategory.spec_getChildCatIdsByCatId,
          DbCategory.spec_getCatPostIdsByCatId,
          DbCategory.spec_getCatRecordByCatId,
          DbCategory.spec_checkIfChildCatIsValid,
          DbCategory.spec_getCats,
          DbCategory.spec_getCatId,
          DbAuthor.spec_getAuthorIdByUserId,
          DbAuthor.spec_getPostIdsByAuthorId,
          DbAuthor.spec_getLastAuthorRecord,
          DbAuthor.spec_getAuthorRecords,
          DbAuthor.spec_getUserIdByAuthorId,
          DbAuthor.spec_getAuthorRecord,
          DbAccount.spec_getPasswordRecordByLogin,
          DbAccount.spec_getIsAdminRecordByToken,
          DbAccount.spec_getUserIdRecordByToken,
          DbAccount.spec_checkAuthorWritePerm,
          DbAccount.spec_checkUserPerm,
          DbAccount.spec_checkAdminPerm,
          DbAccount.spec_checkPassword,
          DbPost.spec_getPostDraftIdsByPostIds,
          DbPost.spec_getPostDraftIdByPostId,
          DbPost.spec_getPostAddPhotoIdsByPostId,
          DbPost.spec_getPostMainPhotoIdByPostId,
          DbPost.spec_getPostTagIdsByPostId,
          DbPost.spec_getPostCategoryIdByPostId,
          DbPost.spec_getPostAuthorIdByPostId,
          DbPost.spec_getPostIdByTitle,
          DbPost.spec_getLastPostRecord,
          DbPost.spec_getPostRecord,
          Util.spec_sqlDAtoText,
          Util.spec_sqlAtoText,
          Util.spec_readKey,
          Query.spec_lookupOptional,
          Query.spec_extractOptional,
          Query.spec_createOptionalDict,
          Query.spec_lookupRequired,
          Query.spec_readRequired,
          Config.spec_checkConfig
        ]
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Specs" specs
        ]
    )
