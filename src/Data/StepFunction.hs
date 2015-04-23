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
import Data.Maybe    (fromMaybe)

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
mkStepFunction x xs = StepFunction x $ simplify $ sort xs

leq :: Ord x
    => Transition x y
    -> x
    -> Bool
leq trans x = x_val trans <= x

-- | Get the y value for a given x
valAt' :: Ord x
       => x
       -> StepFunction x y
       -> (y,Maybe y)
valAt' x (StepFunction def trans) =
  case reverse $ takeWhile (`leq` x) trans of
    [] -> (def,Nothing)  
    [h] -> if left_closed h || x_val h < x then
             (y_val h,Nothing)
           else 
             (def,Just $ y_val h)
    (h:h':_) -> if left_closed h || x_val h < x then 
                  (y_val h,Nothing) 
                else 
                  (y_val h',Just $ y_val h)

valAt :: Ord x
      => x
      -> StepFunction x y
      -> y
valAt x st = fst $ valAt' x st

interleaveSorted :: Ord a
                 => [a]
                 -> [a]
                 -> [a]
interleaveSorted xs []                     = xs
interleaveSorted [] ys                     = ys
interleaveSorted (x:xs) (y:ys) | x <= y    = x:(interleaveSorted xs (y:ys))
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
          if left_closed trans then
            Transition (x_val trans) (f (valAt (x_val trans) s1) $ y_val trans) True
          else
            Transition (x_val trans) (f ((uncurry fromMaybe) $ valAt' (x_val trans) s1) $ y_val trans) False
        mergeS2 trans =
          if left_closed trans then
            Transition (x_val trans) (f (y_val trans) $ valAt (x_val trans) s2) True
          else
            Transition (x_val trans) (f (y_val trans) $ (uncurry fromMaybe) $ valAt' (x_val trans) s2) False

simplify :: Eq y
         => [Transition x y]
         -> [Transition x y]
simplify = concat . map (take 1) . groupBy ((==) `on` y_val)
