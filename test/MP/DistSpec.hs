module MP.DistSpec where

import Data.Ratio
    ( (%)
    )
import qualified Data.Set as S

import Test.Hspec

import MP.Dist

spec :: Spec
spec = do
    describe "throwing dice" $ do
        let die :: [Int]
            die = [1..6]
            dist :: Dist Int
            dist = prob2dist $ do
                d1 <- uniform die
                d2 <- uniform die
                return $ d1 + d2
        it "result" $
            dist `shouldBe` distribution [ (2, 1 % 36), (3, 2 % 36), (4, 3 % 36), (5, 4 % 36)
                                         , (6, 5 % 36), (7, 6 % 36), (8, 5 % 36), (9, 4 % 36)
                                         , (10, 3 % 36), (11, 2 % 36), (12, 1 % 36)
                                         ]
    describe "Monty Hall problem" $ do
        let doors :: S.Set String
            doors = S.fromList ["a", "b", "c"]
            dist1 :: Dist String
            dist1 = prob2dist $ do
                prize <- uniform doors
                choice <- uniform doors
                return $ if prize == choice
                    then "win"
                    else "lose"
            dist2 :: Dist String
            dist2 = prob2dist $ do
                prize <- uniform doors
                choice <- uniform doors
                opened <- uniform . S.difference doors $ S.fromList [prize, choice]
                choice' <- uniform . S.difference doors $ S.fromList [opened, choice]
                return $ if prize == choice'
                    then "win"
                    else "lose"
        it "first choice" $
            dist1 `shouldBe` distribution [("win", 1 % 3), ("lose", 2 % 3)]
        it "second choice" $
            dist2 `shouldBe` distribution [("win", 2 % 3), ("lose", 1 % 3)]
