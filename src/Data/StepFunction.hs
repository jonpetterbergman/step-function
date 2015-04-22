{-# LANGUAGE TupleSections #-}
module Data.StepFunction
  ( mkStepFunction
  , StepFunction
  , valAt
  , merge ) where

import Data.List     (sort,
                      unfoldr,
                      mapAccumL,
                      groupBy)
import Data.Function (on)

data Transition x y =
  Transition 
    {
      x_val :: x
    , y_val :: y
    , left_closed :: Bool
    } deriving (Eq,Show)

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

-- the following should be true:
-- valAt x (merge f sf1 sf2) == f (valAt x sf1) (valAt x sf2)
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
