module MP.DistSpec where

import Data.Ratio
    ( (%)
    )
import qualified Data.Set as S

import qualified Data.Map.Strict as M
import Test.Hspec

import MP.Dist

spec :: Spec
spec = do
    describe "throwing dice" $ do
        let die :: [Int]
            die = [1..6]
            probs :: Probs Int
            probs = dist2probs $ do
                d1 <- uniform die
                d2 <- uniform die
                return $ d1 + d2
        it "result" $
            probs `shouldBe` M.fromList [ (2, 1 % 36), (3, 2 % 36), (4, 3 % 36), (5, 4 % 36)
                                        , (6, 5 % 36), (7, 6 % 36), (8, 5 % 36), (9, 4 % 36)
                                        , (10, 3 % 36), (11, 2 % 36), (12, 1 % 36)
                                        ]
    describe "Monty Hall problem" $ do
        let doors :: S.Set String
            doors = S.fromList ["a", "b", "c"]
            probs1 :: Probs String
            probs1 = dist2probs $ do
                prize <- uniform doors
                choice <- uniform doors
                return $ if prize == choice
                    then "win"
                    else "lose"
            probs2 :: Probs String
            probs2 = dist2probs $ do
                prize <- uniform doors
                choice <- uniform doors
                opened <- uniform . S.difference doors $ S.fromList [prize, choice]
                choice' <- uniform . S.difference doors $ S.fromList [opened, choice]
                return $ if prize == choice'
                    then "win"
                    else "lose"
        it "first choice" $
            probs1 `shouldBe` M.fromList [("win", 1 % 3), ("lose", 2 % 3)]
        it "second choice" $
            probs2 `shouldBe` M.fromList [("win", 2 % 3), ("lose", 1 % 3)]
