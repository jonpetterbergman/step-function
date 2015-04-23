module Merge where

import           Distribution.TestSuite                    (Test)
import           Distribution.TestSuite.QuickCheck         (testProperty)
import           Data.StepFunction                         (StepFunction,
                                                            mkStepFunction,
                                                            Transition(..),
                                                            valAt,
                                                            merge)
import           Test.QuickCheck                           (quickCheck)
import           Test.QuickCheck.Arbitrary                 (Arbitrary(..))
import           Test.QuickCheck.Property                  ((===),
                                                            Property,
                                                            counterexample)

instance (Arbitrary x,Arbitrary y) => Arbitrary (Transition x y) where
  arbitrary = Transition <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary x,Arbitrary y, Eq y, Ord x) => Arbitrary (StepFunction x y) where
  arbitrary = mkStepFunction <$> arbitrary <*> arbitrary

mergeProp :: (Eq c, Ord x, Show c,Show x)
          => (a -> b -> c)
          -> x
          -> StepFunction x a
          -> StepFunction x b
          -> Property
mergeProp f x sf1 sf2 =
  let merged = merge f sf1 sf2 in
  counterexample ("merged: " ++ show merged) $
  valAt x merged === f (valAt x sf1) (valAt x sf2)

--tests :: [Test]
--tests = 
--  [testProperty "merge: valAt x (merge f sf1 sf2) == f (valAt x sf1) (valAt x sf2)"
--   ((\f x sf1 sf2 -> valAt x (merge f sf1 sf2) == f (valAt x sf1) (valAt x sf2)))]   

