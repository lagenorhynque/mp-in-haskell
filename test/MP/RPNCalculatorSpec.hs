module MP.RPNCalculatorSpec where

import Control.Exception
    ( evaluate
    )

import Test.Hspec

import MP.RPNCalculator
    ( solveRPN
    , solveRPN'
    )

spec :: Spec
spec = do
    describe "without monads" $ do
        it "calculates" $
            solveRPN "1 2 * 4 +" `shouldBe` 6.0
        it "calculates" $
            solveRPN "1 2 * 4 + 5 *" `shouldBe` 30.0
        it "returns the last term" $
            solveRPN "1 2 * 4" `shouldBe` 4.0
        it "throws an error" $
          evaluate (solveRPN "1 8 wharglbllargh") `shouldThrow` anyErrorCall
    describe "with Maybe monad" $ do
        it "calculates" $
            solveRPN' "1 2 * 4 +" `shouldBe` Just 6.0
        it "calculates" $
            solveRPN' "1 2 * 4 + 5 *" `shouldBe` Just 30.0
        it "returns Nothing" $
            solveRPN' "1 2 * 4" `shouldBe` Nothing
        it "returns Nothing" $
            solveRPN' "1 8 wharglbllargh" `shouldBe` Nothing
