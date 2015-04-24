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

instance Functor (Transition x) where
  fmap f (Transition x y lc) = Transition x (f y) lc

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
  StepFunction newDef $ simplify $ mergeT f (def s1,def s2) (transitions s1) (transitions s2)
  where newDef = f (def s1) (def s2)

x_pos :: Transition x y
      -> (x,Bool)
x_pos t = (x_val t,not $ left_closed t)

mergeT :: Ord x
       => (a -> b -> c)
       -> (a,b)
       -> [Transition x a]
       -> [Transition x b]
       -> [Transition x c]
mergeT _ _       []     []             = []
mergeT f (_,acc) as     []             = map (fmap (`f` acc)) as
mergeT f (acc,_) []     bs             = map (fmap (acc `f`)) bs
mergeT f acc     (a:at) (b:bt) | x_pos a < x_pos b = mergeLeft f acc a at (b:bt)
                               | x_pos a > x_pos b = mergeRight f acc b (a:at) bt 
                               | otherwise = mergeBoth f a b at bt

mergeLeft f (a_acc,b_acc) a as bs =
  let nval = f (y_val a) b_acc
      ntrans = Transition (x_val a) nval (left_closed a) in
  ntrans:(mergeT f (y_val a,b_acc) as bs)

mergeRight f (a_acc,b_acc) b as bs =
  let nval = f a_acc (y_val b)
      ntrans = Transition (x_val b) nval (left_closed b) in
   ntrans:(mergeT f (a_acc,y_val b) as bs)

mergeBoth f a b as bs =
  let nval = f (y_val a) (y_val b)
      ntrans = Transition (x_val a) nval (left_closed a) in
  ntrans:(mergeT f (y_val a,y_val b) as bs)

simplify :: Eq y
         => [Transition x y]
         -> [Transition x y]
simplify = concat . map (take 1) . groupBy ((==) `on` y_val)
