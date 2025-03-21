module Main where

import Test.Hspec
import Lsystem.Types

-- 必ず1を返す関数
alwaysOne :: Int
alwaysOne = 1

main :: IO ()
main = hspec $ do
    describe "Symbol Smart Constructors" $ do
        describe "makeVariable" $ do
            it "大文字をVariableに変換できる" $ do
                fmap tmp (makeVariable 'A') `shouldBe` Right True
            it "小文字をVariableに変換したらエラー" $ do
                fmap tmp (makeVariable 'b') `shouldBe` Left (InvalidVariable 'b')
tmp :: Symbol -> Bool
tmp (Var _) = True
tmp _ = False
