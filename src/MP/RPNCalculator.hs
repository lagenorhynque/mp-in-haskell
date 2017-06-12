module MP.RPNCalculator ( solveRPN
                        , solveRPN'
                        ) where

import Control.Monad ( foldM, liftM )

-- without monads

foldingFunction :: (Num a, Read a) => [a] -> String -> [a]
foldingFunction (x:y:ys) "+" = (y + x) : ys
foldingFunction (x:y:ys) "-" = (y - x) : ys
foldingFunction (x:y:ys) "*" = (y * x) : ys
foldingFunction xs       s   = read s  : xs

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words

-- with Maybe monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x:y:ys) "+" = return $ (y + x) : ys
foldingFunction' (x:y:ys) "-" = return $ (y - x) : ys
foldingFunction' (x:y:ys) "*" = return $ (y * x) : ys
foldingFunction' xs       s   = liftM (:xs) $ readMaybe s

solveRPN' :: String -> Maybe Double
solveRPN' s = do
    [result] <- foldM foldingFunction' [] $ words s
    return result
