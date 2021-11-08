import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.Hspec (testSpecs)

import qualified TestPost.DB.DBQSpec as DBQSpec
import qualified TestPost.DB.Tag as DBTag
import qualified TestPost.DB.User as DBUser
import qualified TestPost.DB.Photo as DBPhoto
import qualified TestPost.DB.Draft as DBDraft
import qualified TestPost.DB.Comment as DBComment
import qualified TestPost.DB.Category as DBCategory
import qualified TestPost.DB.Author as DBAuthor
import qualified TestPost.DB.Account as DBAccount
import qualified TestPost.DB.Post as DBPost
import qualified TestPost.Server.Util as Util
import qualified TestPost.Server.QueryParameters as Query
import qualified TestPost.Config as Config

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ 
      DBQSpec.spec_queryFromWhere,
      DBQSpec.spec_queryFromWhereIn,
      DBQSpec.spec_queryFromWhereInLimit,
      DBQSpec.spec_queryFromOrderLimitOffset,
      DBQSpec.spec_queryFromOrderLimit,
      DBQSpec.spec_queryDeleteWhere,
      DBQSpec.spec_queryInsertIntoValues,
      DBQSpec.spec_queryUpdateSetWhere,
      DBQSpec.spec_querySpecialPosts,
      DBQSpec.spec_querySearchPost,
      DBQSpec.spec_querySearchCat,
      DBQSpec.spec_querySearchTag,
      DBQSpec.spec_querySearchAuthor,
      DBQSpec.spec_findInPosts,
      DBQSpec.spec_findInAuthors,
      DBQSpec.spec_findInCats,
      DBQSpec.spec_findInTags,
      DBQSpec.spec_querySort,
      DBTag.spec_newTag,
      DBTag.spec_getTagPostRecords,
      DBTag.spec_getTagRecordsById,
      DBTag.spec_getAllTagRecords,
      DBTag.spec_getTagIdByTitle,
      DBUser.spec_newUser,
      DBUser.spec_getAuthorIdByUserId,
      DBUser.spec_getUserRecords,
      DBUser.spec_getUserRecordById,
      DBUser.spec_getUserIdByLogin,
      DBPhoto.spec_newPhoto,
      DBPhoto.spec_getLastPhotoRecord,
      DBPhoto.spec_getPhotoRecordById,
      DBPhoto.spec_getPhotoIdByName,
      DBDraft.spec_newDraft,
      DBDraft.spec_getDraftText,
      DBDraft.spec_getDraftRecords,
      DBComment.spec_newComment,
      DBComment.spec_getLastCommentRecord,
      DBComment.spec_getCommentRecord,
      DBCategory.spec_getSub,
      DBCategory.spec_newCatNull,
      DBCategory.spec_newCat,
      DBCategory.spec_getChildCatIdsByCatId,
      DBCategory.spec_getCatPostIdsByCatId,
      DBCategory.spec_getCatRecordByCatId,
      DBCategory.spec_checkIfChildCatIsValid,
      DBCategory.spec_getCats,
      DBCategory.spec_getCatId,
      DBAuthor.spec_getAuthorIdByUserId,
      DBAuthor.spec_getPostIdsByAuthorId,
      DBAuthor.spec_getLastAuthorRecord,
      DBAuthor.spec_getAuthorRecords,
      DBAuthor.spec_getUserIdByAuthorId,
      DBAuthor.spec_getAuthorRecord,
      DBAccount.spec_getPasswordRecordByLogin,
      DBAccount.spec_getIsAdminRecordByToken,
      DBAccount.spec_getUserIdRecordByToken,
      DBAccount.spec_checkAuthorWritePerm,
      DBAccount.spec_checkUserPerm,
      DBAccount.spec_checkAdminPerm,
      DBAccount.spec_checkPassword,
      DBPost.spec_getPostDraftIdsByPostIds,
      DBPost.spec_getPostDraftIdByPostId,
      DBPost.spec_getPostAddPhotoIdsByPostId,
      DBPost.spec_getPostMainPhotoIdByPostId,
      DBPost.spec_getPostTagIdsByPostId,
      DBPost.spec_getPostCategoryIdByPostId,
      DBPost.spec_getPostAuthorIdByPostId,
      DBPost.spec_getPostIdByTitle,
      DBPost.spec_getLastPostRecord,
      DBPost.spec_getPostRecord,
      Util.spec_sqlDAtoText,
      Util.spec_sqlAtoText,
      Util.spec_readEitherMa,
      Query.spec_lookupOptionalParam,
      Query.spec_extractOptional,
      Query.spec_createOptionalDict,
      Query.spec_lookupReqParam,
      Query.spec_extractRequired,
      Config.spec_checkConfig
    ]
  defaultMain (testGroup "All Tests" [
    testGroup "Specs" specs
    ])