import Test.QuickCheck

import qualified Data.Group.Permutation.Permutation.Tests as Perm
import qualified Data.Group.Permutation.Group.Tests as Group
import qualified Data.Group.Permutation.Primitive.Tests as Prim

tests = sized $ \ m -> resize (floor (logBase 2 (fromIntegral m))) $ conjoin
  [printTestCase "Permutation" $ Perm.tests,
    printTestCase "Group" $ Group.tests,
    printTestCase "Primitive" $ Prim.tests]

main = quickCheck tests
