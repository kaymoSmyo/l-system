module Main where

import Test.Hspec
import Lsystem.Types
import Lsystem.Parser
import Lsystem.Core

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
            fmap forparser (runParser parseVariabe "A") `shouldBe` Right True
            runParser parseVariabe "a" `shouldBe` []
            runParser parseVariabe "+" `shouldBe` []
        it "parseConstant" $ do
            runParser parseConstant "A" `shouldBe` []
            constant2Bool (fst . head $ runParser parseConstant "a") `shouldBe` True
            runParser parseConstant "+" `shouldBe` []
        it "parserOperator" $ do
            runParser parseOperator "A" `shouldBe` []
            runParser parseOperator "a" `shouldBe` []
            operator2Bool (fst . head $ runParser parseOperator "+") `shouldBe` True
    describe "e2Movesのテスト" $ do
        it "succsess" $ do
            length (e2Moves []) `shouldBe` 0
            length (e2Moves . fsthead $ runParser parseExpression "+A") `shouldBe` 1
        it "succsess2" $ do
            let angle = pi/2
            let moves = map (\f -> f angle) (e2Moves . fsthead $ runParser parseExpression "+A")
            let finalPos = scanl (\pos f -> f pos) (0, 0) moves
            finalPos `shouldBe` scanl (\pos f -> f pos) (0, 0) [moveFoward 1 angle]
        it "succsess3" $ do
            let angle = pi/2
            let moves = map (\f -> f angle) (e2Moves . fsthead $ runParser parseExpression "-A")
            let finalPos = scanl (\pos f -> f pos) (0, 0) moves
            finalPos `shouldBe` scanl (\pos f -> f pos) (0, 0) [moveFoward (-1) angle]
        it "succsess3" $ do
            let angle = pi/3
            let moves = map (\f -> f angle) (e2Moves . fsthead $ runParser parseExpression "A + B - - A + B + + - -")
            let poss = scanl (\pos f -> f pos) (0, 0) moves
            poss `shouldBe` 
                scanl (\pos f -> f pos) (0, 0) 
                    (map (\f -> f angle) [moveFoward 0, moveFoward 1, moveFoward (-1), moveFoward 0])
            
variable2Bool :: Symbol -> Bool
variable2Bool (Var _) = True
variable2Bool _ = False
constant2Bool :: Symbol -> Bool
constant2Bool (Const _) = True
constant2Bool _ = False
operator2Bool :: Symbol -> Bool
operator2Bool (OP _) = True
operator2Bool _ = False
fsthead = fst . head
forparser :: (Symbol, a) -> Bool
forparser (Var _, _) = True
forparser _ = False