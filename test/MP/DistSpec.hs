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
            dist1 :: Probs
            dist1 = dist2probs $ do
                prize <- uniform $ S.toList doors
                choice <- uniform $ S.toList doors
                return $ if prize == choice
                    then "win"
                    else "lose"
            dist2 :: Probs
            dist2 = dist2probs $ do
                prize <- uniform $ S.toList doors
                choice <- uniform $ S.toList doors
                opened <- uniform . S.toList . S.difference doors $ S.fromList [prize, choice]
                choice' <- uniform . S.toList . S.difference doors $ S.fromList [opened, choice]
                return $ if prize == choice'
                    then "win"
                    else "lose"
        it "dist1" $
            dist1 `shouldBe` M.fromList [("win", 1 % 3), ("lose", 2 % 3)]
        it "dist2" $
            dist2 `shouldBe` M.fromList [("win", 2 % 3), ("lose", 1 % 3)]
