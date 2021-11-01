import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.Hspec (testSpecs)

import qualified TestPost.DB.DBQSpec as DBQSpec
import qualified TestPost.DB.Tag as DBT
import qualified TestPost.DB.User as DBU
import qualified TestPost.DB.Photo as DBPh
import qualified TestPost.DB.Draft as DBD
import qualified TestPost.DB.Comment as DBCo
import qualified TestPost.DB.Category as DBC
import qualified TestPost.DB.Author as DBA
import qualified TestPost.DB.Account as DBAc
import qualified TestPost.DB.Post as DBP
import qualified TestPost.Server.Util as Util
import qualified TestPost.Server.QueryParameters as QP
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
               DBT.spec_newTag,
               DBT.spec_getTagPostRecords,
               DBT.spec_getTagRecordsById,
               DBT.spec_getAllTagRecords,
               DBT.spec_getTagIdByTitle,
               DBU.spec_newUser,
               DBU.spec_getAuthorIdByUserId,
               DBU.spec_getUserRecords,
               DBU.spec_getUserRecordbyId,
               DBU.spec_getUserIdByLogin,
               DBPh.spec_newPhoto,
               DBPh.spec_getLastPhotoRecord,
               DBPh.spec_getPhotoRecordById,
               DBPh.spec_getPhotoIdByName,
               DBD.spec_newDraft,
               DBD.spec_getDraftText,
               DBD.spec_getDraftRecords,
               DBCo.spec_newComment,
               DBCo.spec_getLastCommentRecord,
               DBCo.spec_getCommentRecord,
               DBC.spec_getSub,
               DBC.spec_newCatNull,
               DBC.spec_newCat,
               DBC.spec_getChildCatIdsByCatId,
               DBC.spec_getCatPostIdsByCatId,
               DBC.spec_getCatRecordByCatId,
               DBC.spec_checkIfChildCatIsValid,
               DBC.spec_getCats,
               DBC.spec_getCatId,
               DBA.spec_getAuthorIdByUserId,
               DBA.spec_getPostIdsByAuthorId,
               DBA.spec_getLastAuthorRecord,
               DBA.spec_getAuthorRecords,
               DBA.spec_getUserIdByAuthorId,
               DBA.spec_getAuthorRecord,
               DBAc.spec_getPasswordRecordByLogin,
               DBAc.spec_getIsAdminRecordByToken,
               DBAc.spec_getUserIdRecordByToken,
               DBAc.spec_checkAuthorWritePerm,
               DBAc.spec_checkUserPerm,
               DBAc.spec_checkAdminPerm,
               DBAc.spec_checkPassword,
               DBP.spec_getPostDraftIdsByPostIds,
               DBP.spec_getPostDraftIdByPostId,
               DBP.spec_getPostAddPhotoIdsByPostId,
               DBP.spec_getPostMainPhotoIdByPostId,
               DBP.spec_getPostTagIdsByPostId,
               DBP.spec_getPostCategoryIdByPostId,
               DBP.spec_getPostAuthorIdbyPostId,
               DBP.spec_getPostIdByTitle,
               DBP.spec_getLastPostRecord,
               DBP.spec_getPostRecord,
               Util.spec_sqlDAtoText,
               Util.spec_sqlAtoText,
               Util.spec_readEitherMa,
               QP.spec_lookupOptionalParam,
               QP.spec_extractOptional,
               QP.spec_createOptionalDict,
               QP.spec_lookupReqParam,
               QP.spec_extractRequired,
               Config.spec_checkConfig
             ]
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs
                ])