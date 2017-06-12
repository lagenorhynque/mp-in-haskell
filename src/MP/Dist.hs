module MP.Dist where

import Control.Applicative
    ( Applicative
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Ratio
    ( Ratio,
      (%)
    )

import qualified Data.Map.Strict as M

newtype Dist a = Dist { getDist :: [(a, Ratio Integer)] } deriving (Show)

instance Functor Dist where
    fmap f (Dist xs) = Dist $ map (\(x, p) -> (f x, p)) xs

instance Applicative Dist where
    pure v = Dist [(v, 1)]
    Dist f <*> Dist xs = Dist $ f >>= (\(g, p) -> map (\(x', q) -> (g x', p * q)) xs)

instance Monad Dist where
    m >>= f = Dist [(y, p * q) | (x, p) <- getDist m,
                                 (y, q) <- getDist $ f x]
{-
    m >>= f = flatten $ fmap f m
      where
        flatten (Dist xs) = Dist $ concatMap multAll xs
          where
            multAll (Dist innerxs, p) = map (\(x, q) -> (x, p * q)) innerxs
-}

uniform :: [a] -> Dist a
uniform xs = Dist $ map (\x -> (x, 1 % n)) xs
  where
    n = fromIntegral $ length xs

type Probs = M.Map String (Ratio Integer)

dist2probs :: Dist String -> Probs
dist2probs (Dist xs) = foldr addProb M.empty xs
  where
    addProb (x, p) acc = M.insert x (fromMaybe 0 (M.lookup x acc) + p) acc
