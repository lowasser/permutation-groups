import Test.QuickCheck

import qualified Data.Group.Permutation.Permutation.Tests as PermTests

tests = conjoin
  [printTestCase "Permutation" $ conjoin PermTests.tests]

main = quickCheck tests
