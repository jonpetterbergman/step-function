{-# LANGUAGE TupleSections #-}
-- | 
-- Functions for dealing with step functions

module Data.StepFunction
  ( Transition(..)
  , StepFunction
  , mkStepFunction
  , valAt
  , merge ) where

import Data.List     (sort,
                      unfoldr,
                      mapAccumL,
                      groupBy)
import Data.Function (on)

-- | A Transition, for a certain value on the x axis, there is a new y value.
data Transition x y =
  Transition 
    {
      x_val :: x -- ^ The x value where the transition happens
    , y_val :: y -- ^ The new y value
    , left_closed :: Bool -- ^ If True, y_val is for all x >= x_val, otherwise for all x > x_val
    } deriving (Eq,Show)

-- | A StepFunction is implemented as a default value
-- and a sorted list of Transitions
data StepFunction x y =
  StepFunction 
    {
      def :: y
    , transitions :: [Transition x y]
    } deriving (Eq,Show)

instance (Ord x,Eq y) => Ord (Transition x y) where
  compare t1 t2 | x_val t1 < x_val t2                                              = LT
                | x_val t1 > x_val t2                                              = GT
                | x_val t1 == x_val t2 && left_closed t1 && (not $ left_closed t2) = LT
                | x_val t1 == x_val t2 && (not $ left_closed t1) && left_closed t2 = GT
                | otherwise                                                        = EQ

-- | Smart constructor sorts the list of transitions
mkStepFunction :: (Ord x,Eq y)
               => y
               -> [Transition x y]
               -> StepFunction x y
mkStepFunction x xs = StepFunction x $ sort xs

rightOf :: Ord x
        => x
        -> Transition x y
        -> Bool
rightOf x trans | x_val trans < x                       = True
                | x_val trans == x && left_closed trans = True
                | otherwise                             = False 

-- | Get the y value for a given x
valAt :: Ord x
      => x
      -> StepFunction x y
      -> y
valAt x (StepFunction def trans) =
  case reverse $ takeWhile (rightOf x) trans of
    [] -> def  
    (h:_) -> y_val h

interleaveSorted :: Ord a
                 => [a]
                 -> [a]
                 -> [a]
interleaveSorted xs []                     = xs
interleaveSorted [] ys                     = ys
interleaveSorted (x:xs) (y:ys) | x < y     = x:(interleaveSorted xs (y:ys))
                               | otherwise = y:(interleaveSorted (x:xs) ys) 

-- | Merge two step function, such that the following should be true:
--
-- > valAt x (merge f sf1 sf2) == f (valAt x sf1) (valAt x sf2)
--
-- The resulting step function will be simplified, transitions that
-- don't change the y value will be eliminated.
merge :: (Ord x,Eq c)
      => (a -> b -> c)
      -> StepFunction x a
      -> StepFunction x b
      -> StepFunction x c
merge f s1 s2 = 
  StepFunction newDef $ simplify $ interleaveSorted (map mergeS2 $ transitions s1)
                                                    (map mergeS1 $ transitions s2)
  where newDef = f (def s1) (def s2)
        mergeS1 trans =
          Transition (x_val trans) (f (valAt (x_val trans) s1) $ y_val trans) (left_closed trans)
        mergeS2 trans =
          Transition (x_val trans) (f (y_val trans) $ valAt (x_val trans) s2) (left_closed trans)
        simplify = concat . map (take 1) . groupBy ((==) `on` y_val)
