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
            fmap variable2Bool (makeVariable 'A') `shouldBe` Right True
            fmap variable2Bool (makeVariable 'a') `shouldBe` Left (InvalidVariable 'a')
        it "makeContestant" $ do
            fmap constant2Bool (makeConstant 'a') `shouldBe` Right True
            fmap constant2Bool (makeConstant 'A') `shouldBe` Left (InvalidConstant 'A')
        it "makeContestant" $ do
            fmap operator2Bool (makeOperator '+') `shouldBe` Right True
            fmap operator2Bool (makeOperator 'A') `shouldBe` Left (InvalidOperator 'A')
            fmap operator2Bool (makeOperator 'a') `shouldBe` Left (InvalidOperator 'a')

    describe "Variable/Constant/Operatorのパーサ" $ do
        it "parseVariable" $ do
            (variable2Bool $ fst . head $ runParser parseVariabe "A") `shouldBe` True
            runParser parseVariabe "a" `shouldBe` []
            runParser parseVariabe "+" `shouldBe` []
        it "parseConstant" $ do
            runParser parseConstant "A" `shouldBe` []
            (constant2Bool $ fst . head $ runParser parseConstant "a") `shouldBe` True
            runParser parseConstant "+" `shouldBe` []
        it "parserOperator" $ do
            runParser parseOperator "A" `shouldBe` []
            runParser parseOperator "a" `shouldBe` []
            (operator2Bool $ fst . head $ runParser parseOperator "+") `shouldBe` True

variable2Bool :: Symbol -> Bool
variable2Bool (Var _) = True
variable2Bool _ = False
constant2Bool :: Symbol -> Bool
constant2Bool (Const _) = True
constant2Bool _ = False
operator2Bool :: Symbol -> Bool
operator2Bool (OP _) = True
operator2Bool _ = False