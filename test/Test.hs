import Test.Tasty
import Test.Tasty.Hspec
--import Test.Tasty.Hedgehog

import qualified TestPost.DB.DBQSpec as DBQSpec

main :: IO ()
--main = defaultMain $ testGroup "(no tests)" []
main = do
  specs <- concat <$> mapM testSpecs
             [ 
               DBQSpec.spec_queryFromWhere,
               DBQSpec.spec_queryFromWhereIn,
               DBQSpec.spec_queryFrom,
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
               DBQSpec.spec_querySort
             ]
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs
                --  testGroup "Properties" props
                --, testGroup "Golden Tests" goldens
                ])