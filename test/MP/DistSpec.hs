module MP.DistSpec where

import Data.Ratio
    ( (%)
    )
import qualified Data.Set as S

import qualified Data.Map.Strict as M
import Test.Hspec

import MP.Dist

spec :: Spec
spec =
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
        it "pattern 1" $
            probs1 `shouldBe` M.fromList [("win", 1 % 3), ("lose", 2 % 3)]
        it "pattern 2" $
            probs2 `shouldBe` M.fromList [("win", 2 % 3), ("lose", 1 % 3)]
