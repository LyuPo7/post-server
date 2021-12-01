import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.Hspec (testSpecs)

import qualified TestPost.Db.DbQSpec as DbQSpec
import qualified TestPost.Db.Tag as DbTag
import qualified TestPost.Db.User as DbUser
import qualified TestPost.Db.Photo as DbPhoto
import qualified TestPost.Db.Draft as DbDraft
import qualified TestPost.Db.Comment as DbComment
import qualified TestPost.Db.Category as DbCategory
import qualified TestPost.Db.Author as DbAuthor
import qualified TestPost.Db.Account as DbAccount
import qualified TestPost.Db.Post as DbPost
import qualified TestPost.Server.Util as Util
import qualified TestPost.Server.QueryParameters as Query
import qualified TestPost.Config as Config

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ 
      DbQSpec.spec_queryFromWhere,
      DbQSpec.spec_queryFromWhereIn,
      DbQSpec.spec_queryFromWhereInLimit,
      DbQSpec.spec_queryFromOrderLimitOffset,
      DbQSpec.spec_queryFromOrderLimit,
      DbQSpec.spec_queryDeleteWhere,
      DbQSpec.spec_queryInsertIntoValues,
      DbQSpec.spec_queryUpdateSetWhere,
      DbQSpec.spec_querySpecialPosts,
      DbQSpec.spec_querySearchPost,
      DbQSpec.spec_querySearchCat,
      DbQSpec.spec_querySearchTag,
      DbQSpec.spec_querySearchAuthor,
      DbQSpec.spec_findInPosts,
      DbQSpec.spec_findInAuthors,
      DbQSpec.spec_findInCats,
      DbQSpec.spec_findInTags,
      DbQSpec.spec_querySort,
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
  defaultMain (testGroup "All Tests" [
    testGroup "Specs" specs
    ])