module MP.Dist
    ( Dist
    , Prob(..)
    , distribution
    , prob2dist
    , uniform
    ) where

import Control.Applicative
    ( Applicative
    )
import Data.Foldable
    ( toList
    )
import Data.Ratio
    ( Rational,
      (%)
    )

import qualified Data.Map.Strict as M

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show, Eq)

instance Functor Prob where
    fmap f (Prob xs) = Prob [(f x, p) | (x, p) <- xs]

instance Applicative Prob where
    pure v = Prob [(v, 1)]
    Prob f <*> Prob xs = Prob [(g x, p * q) | (g, p) <- f
                                            , (x, q) <- xs]

instance Monad Prob where
    Prob xs >>= f = Prob [(y, p * q) | (x, p) <- xs
                                     , (y, q) <- getProb $ f x]

uniform :: (Foldable f) => f a -> Prob a
uniform xs = Prob . map (\x -> (x, 1 % n)) $ toList xs
  where
    n = fromIntegral $ length xs

type Dist a = M.Map a Rational

prob2dist :: (Ord a) => Prob a -> Dist a
prob2dist (Prob xs) = foldr addProb M.empty xs
  where
    addProb (x, p) = M.insertWith (+) x p

distribution :: (Ord a) => [(a, Rational)] -> Dist a
distribution = M.fromList
