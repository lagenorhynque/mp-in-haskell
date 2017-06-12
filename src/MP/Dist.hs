module MP.Dist
    ( Dist
    , Probs
    , dist2probs
    , uniform
    ) where

import Control.Applicative
    ( Applicative
    )
import Data.Ratio
    ( Ratio,
      (%)
    )
import qualified Data.Set as S

import qualified Data.Map.Strict as M

newtype Dist a = Dist { getDist :: [(a, Ratio Integer)] } deriving (Show)

instance Functor Dist where
    fmap f (Dist xs) = Dist [(f x, p) | (x, p) <- xs]

instance Applicative Dist where
    pure v = Dist [(v, 1)]
    Dist f <*> Dist xs = Dist [(g x, p * q) | (g, p) <- f
                                            , (x, q) <- xs]

instance Monad Dist where
    Dist xs >>= f = Dist [(y, p * q) | (x, p) <- xs
                                     , (y, q) <- getDist $ f x]

uniform :: S.Set a -> Dist a
uniform xs = Dist . map (\x -> (x, 1 % n)) $ S.toList xs
  where
    n = fromIntegral $ length xs

type Probs a = M.Map a (Ratio Integer)

dist2probs :: (Ord a) => Dist a -> Probs a
dist2probs (Dist xs) = foldr addProb M.empty xs
  where
    addProb (x, p) = M.insertWith (+) x p
