{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

#ifndef MIN_VERSION_transformers_compat
#define MIN_VERSION_transformers_compat(x,y,z) 0
#endif

#if MIN_VERSION_base(4,9,0)
#define LIFTED_FUNCTOR_CLASSES 1

#elif MIN_VERSION_transformers(0,5,0)
#define LIFTED_FUNCTOR_CLASSES 1

#elif MIN_VERSION_transformers_compat(0,5,0) && !MIN_VERSION_transformers(0,4,0)
#define LIFTED_FUNCTOR_CLASSES 1
#endif

module Data.Function.Step.Discrete (
    -- * Step Function
    -- $setup
    SF (..),
    -- * Construction
    constant,
    step,
    fromList,
    -- * Normalisation
    normalise,
    -- * Operators
    (!),
    values,
    -- * Conversions
    toDense,
    fromDense,
    -- * Debug
    showSF,
    putSF,
    ) where

import Control.Applicative   (liftA2)
import Control.DeepSeq       (NFData (..))
import Control.Monad         (ap)
import Data.Bifunctor.Compat (first)
import Data.Functor.Classes
import Data.Map              (Map)
import Prelude ()
import Prelude.Compat

#ifdef LIFTED_FUNCTOR_CLASSES
import Text.Show (showListWith)
#endif

import qualified Data.Function.Step as SF
import qualified Data.Map           as Map
import qualified Test.QuickCheck    as QC

-- | Step function. Piecewise constant function, having finitely many pieces.
-- See <https://en.wikipedia.org/wiki/Step_function>.
--
-- /Note:/ this variant has discrete domain.
-- It's enough to have only @<@$, without @≤@, as there is a /next/ element
-- without any others in between.
--
-- @'SF' (fromList [(k1,v1), (k2,v2)]) v3 :: 'SF' k v@ describes a piecewise constant function \(f : k \to v\):
--
-- \[
-- f\,x = \begin{cases}
-- v_1, \quad x < k_1 \newline
-- v_2, \quad k_1 \le x < k_2 \newline
-- v_3, \quad k_2 \le x
-- \end{cases}
-- \]
--
-- or as you would write in Haskell
--
-- @
-- f x | x < k1    = v1
--     | x < k2    = v2
--     | otherwise = v3
-- @
--
-- There is [step-function](https://hackage.haskell.org/package/step-function) package, but its implementation isn't backed by 'Map'.
--
-- /Note:/ [total-map](https://hackage.haskell.org/package/total-map-0.0.6/docs/Data-TotalMap.html) package,
-- which provides /otherwise constant function with finitely different points/.
--
-- Constructor is exposed as you cannot construct non-valid 'SF'.
--
data SF k v = SF !(Map k v) !v
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | 'pure' is a constant function.
instance Ord k => Applicative (SF k) where
    pure  = constant
    (<*>) = ap

instance Ord k => Monad (SF k) where
    return = pure

    SF m def0 >>= f = SF
        (Map.fromDistinctAscList $ mkDistinctAscList $ pieces ++ pieces1)
        def1
      where
        pieces =
            [ (min k k', v')
            | (k, v) <- Map.toList m
            , let SF m' def = f v
            , (k', v') <- Map.toList m' ++ [(k, def)]
            ]
        (pieces1, def1) = let SF m' def = f def0 in (Map.toList m', def)

-- | Piecewise '<>'.
--
-- >>> putSF $ step 0 "a" "b" <> step 1 "c" "d"
-- \x -> if
--     | x < 0     -> "ac"
--     | x < 1     -> "bc"
--     | otherwise -> "bd"
--
instance (Ord k, Semigroup v) => Semigroup (SF k v) where
    (<>) = liftA2 (<>)

instance (Ord k, Monoid v) => Monoid (SF k v) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v) => QC.Arbitrary (SF k v) where
    arbitrary = fromList <$> QC.arbitrary <*> QC.arbitrary
    shrink (SF m v) = uncurry fromList <$> QC.shrink (Map.toList m, v)

instance (NFData k, NFData v) => NFData (SF k v) where
    rnf (SF m v) = rnf (m, v)

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

#if LIFTED_FUNCTOR_CLASSES
instance Show2 SF where
    liftShowsPrec2 spk slk spv slv d (SF m v) = showsBinaryWith
        (\_ -> showListWith $ liftShowsPrec2 spk slk spv slv 0)
        spv
        "fromList" d (Map.toList m) v

instance Show k => Show1 (SF k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show k, Show v) => Show (SF k v) where
    showsPrec = showsPrec2

#else

instance (Show k, Show v) => Show (SF k v) where
    showsPrec d (SF m v) = showParen (d > 10)
        $ showString "fromList"
        . showsPrec 11 (Map.toList m)
        . showChar ' '
        . showsPrec 11 v

instance Show k => Show1 (SF k) where showsPrec1 = showsPrec

#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mkDistinctAscList :: Ord k => [(k, b)] -> [(k, b)]
mkDistinctAscList []            = []
mkDistinctAscList ((k, v) : kv) = (k, v) : mkDistinctAscList' k kv

mkDistinctAscList' :: Ord k => k -> [(k, b)] -> [(k, b)]
mkDistinctAscList' _ [] = []
mkDistinctAscList' k (p@(k', _) : kv)
    | k < k'    = p : mkDistinctAscList' k' kv
    | otherwise =     mkDistinctAscList' k  kv

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

infixl 9 !

-- | Apply 'SF'.
--
-- >>> heaviside ! 2
-- 1
(!) :: Ord k => SF k v -> k -> v
SF m def ! x = case Map.lookupGT x m of
    Nothing     -> def
    Just (_, v) -> v

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Constant function
--
-- >>> putSF $ constant 1
-- \_ -> 1
--
constant :: a -> SF k a
constant = SF Map.empty

-- | Step function.
--
-- @'step' k v1 v2 = \\ x -> if x < k then v1 else v2@.
--
-- >>> putSF $ step 1 2 3
-- \x -> if
--     | x < 1     -> 2
--     | otherwise -> 3
--
step :: k -> v -> v -> SF k v
step k = SF . Map.singleton k

-- | Create function from list of cases and default value.
--
-- >>> putSF $ fromList [(1,2),(3,4)] 5
-- \x -> if
--     | x < 1     -> 2
--     | x < 3     -> 4
--     | otherwise -> 5
--
-- >>> map (fromList [(1,2),(3,4)] 5 !) [0..10]
-- [2,4,4,5,5,5,5,5,5,5,5]
--
fromList :: Ord k => [(k, v)] -> v -> SF k v
fromList = SF . Map.fromList

-------------------------------------------------------------------------------
-- Conversions to/from list
-------------------------------------------------------------------------------

-- | Possible values of 'SF'
--
-- >>> values heaviside
-- [-1,1]
--
values :: SF k v -> [v]
values (SF m v) = Map.elems m ++ [v]

-------------------------------------------------------------------------------
-- Normalise
-------------------------------------------------------------------------------

-- | Merge adjustent pieces with same values.
--
-- /Note:/ 'SF' isn't normalised on construction.
-- Values don't necessarily are 'Eq'.
--
-- >>> putSF $ normalise heaviside
-- \x -> if
--     | x < 0     -> -1
--     | otherwise -> 1
--
-- >>> putSF $ normalise $ step 0 1 1
-- \_ -> 1
--
-- prop> normalise (liftA2 (+) p (fmap negate p)) == (pure 0 :: SF Int Int)
--
normalise :: Eq v => SF k v -> SF k v
normalise (SF m v) = uncurry mk $ foldr go ([], v) (Map.toList m) where
    mk m' _ = SF (Map.fromDistinctAscList m') v

    go p@(_, v') p'@(m', x)
        | v' == x   = p'
        | otherwise = (p : m', v')

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Convert from discrete variant to more "dense"
--
-- >>> SF.putSF $ toDense $ fromList [(1,2),(3,4)] 5
-- \x -> if
--     | x < 1     -> 2
--     | x < 3     -> 4
--     | otherwise -> 5
--
toDense :: SF a b -> SF.SF a b
toDense (SF m v) = SF.SF (Map.mapKeysMonotonic SF.Open m) v

-- | Convert from "dense" variant. @<= k@ pieces will be converted to @< 'succ' k@.
-- There might be less pieces in the ressult 'SF', than in the original.
--
-- >>> let f = SF.fromList [(SF.Open 1,2),(SF.Closed 3,4),(SF.Open 4,5)] 6
-- >>> SF.putSF f
-- \x -> if
--     | x < 1     -> 2
--     | x <= 3    -> 4
--     | x < 4     -> 5
--     | otherwise -> 6
--
-- >>> putSF $ fromDense f
-- \x -> if
--     | x < 1     -> 2
--     | x < 4     -> 4
--     | otherwise -> 6
--
fromDense :: (Enum a, Ord a) => SF.SF a b -> SF a b
fromDense (SF.SF m v) = SF (mapKeys fk m) v where
    fk (SF.Open k)   = k
    fk (SF.Closed k) = succ k

    mapKeys fk' = Map.fromListWith (\_ -> id) . map (first fk') . Map.toList

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

-- | Show 'SF' as Haskell code
showSF :: (Show a, Show b) => SF a b -> String
showSF (SF m v) | Map.null m = "\\_ -> " ++ show v
showSF (SF m v) = unlines $
    "\\x -> if" : [ "    | " ++ leftPad k ++ " -> " ++ x | (k, x) <- cases ]
  where
    cases     = [ ("x < " ++ show k, show x) | (k,x) <- Map.toList m ] ++
                [ ("otherwise", show v) ]
    len       = maximum (map (length . fst) cases)
    leftPad s = s ++ replicate (len - length s) ' '

-- | @'putStr' . 'showSF'@
putSF :: (Show a, Show b) => SF a b -> IO ()
putSF = putStr . showSF

-- $setup
--
-- == Examples
--
-- >>> let heaviside = step 0 (-1) 1 :: SF Int Int
-- >>> putSF heaviside
-- \x -> if
--     | x < 0     -> -1
--     | otherwise -> 1
--
-- >>> map (heaviside !) [-3, 0, 4]
-- [-1,1,1]
