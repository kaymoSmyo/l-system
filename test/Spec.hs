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
        describe "makeVariable" $ do
            it "大文字をVariableに変換できる" $ do
                fmap variablel2Bool (makeVariable 'A') `shouldBe` Right True
            it "小文字をVariableに変換したらエラー" $ do
                fmap variablel2Bool (makeVariable 'b') `shouldBe` Left (InvalidVariable 'b')
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