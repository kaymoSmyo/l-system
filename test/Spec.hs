module Main where

import Test.Hspec
import Lsystem.Types
import Lsystem.Parser

-- 必ず1を返す関数
alwaysOne :: Int
alwaysOne = 1

main :: IO ()
main = hspec $ do
    describe "Symbol Smart Constructors" $ do
        it "makeVariable" $ do
            fmap variablel2Bool (makeVariable 'A') `shouldBe` Right True
            fmap variablel2Bool (makeVariable 'a') `shouldBe` Left (InvalidVariable 'a')
        it "makeContestant" $ do
            fmap constant2Bool (makeConstant 'a') `shouldBe` Right True
            fmap constant2Bool (makeConstant 'A') `shouldBe` Left (InvalidConstant 'A')
        it "makeContestant" $ do
            fmap operator2Bool (makeOperator '+') `shouldBe` Right True
            fmap operator2Bool (makeOperator 'A') `shouldBe` Left (InvalidOperator 'A')
            fmap operator2Bool (makeOperator 'a') `shouldBe` Left (InvalidOperator 'a')

    -- describe "Variable/Constant/Operatorのパーサ" $ do
    --     describe "Variableパーサ" $ do
    --         it "成功" $ do
    --             fmap variablel2Bool (variable "A+") `shouldBe` True

variablel2Bool :: Symbol -> Bool
variablel2Bool (Var _) = True
variablel2Bool _ = False
constant2Bool :: Symbol -> Bool
constant2Bool (Const _) = True
constant2Bool _ = False
operator2Bool :: Symbol -> Bool
operator2Bool (OP _) = True
operator2Bool _ = False