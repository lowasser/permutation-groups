import Test.QuickCheck

import qualified Data.Group.Permutation.Permutation.Tests as Perm
import qualified Data.Group.Permutation.Group.Tests as Group

tests = conjoin
  [printTestCase "Permutation" $ Perm.tests,
    printTestCase "Group" $ Group.tests]

main = quickCheck tests
