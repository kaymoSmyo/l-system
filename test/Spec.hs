module Main(main) where

import Test.Hspec
import Lsystem.Types
import Lsystem.Parser
import Lsystem.EvalExpr

main :: IO ()
main = hspec $ do
    describe "Symbol Smart Constructors" $ do
        it "makeVariable" $ do
            variable2Bool <$> makeVariable 'A' `shouldBe` Right True
            variable2Bool <$> makeVariable 'a' `shouldBe` Left (InvalidVariable 'a')
        it "makeContestant" $ do
            constant2Bool <$> makeConstant 'a' `shouldBe` Right True
            constant2Bool <$> makeConstant 'A' `shouldBe` Left (InvalidConstant 'A')
        it "makeContestant" $ do
            operator2Bool <$> makeOperator '+' `shouldBe` Right True
            operator2Bool <$> makeOperator 'A' `shouldBe` Left (InvalidOperator 'A')
            operator2Bool <$> makeOperator 'a' `shouldBe` Left (InvalidOperator 'a')

    describe "Variable/Constant/Operatorのパーサ" $ do
        it "parseVariable" $ do
            variable2Bool . fst <$> runParser parseVariabe "A" `shouldBe` Right True
            runParser parseVariabe "a" `shouldBe` Left (MakeSymbolError (InvalidVariable 'a'))
            runParser parseVariabe "+" `shouldBe` Left (MakeSymbolError (InvalidVariable '+'))
        it "parseConstant" $ do
            runParser parseConstant "A" `shouldBe` Left (MakeSymbolError (InvalidConstant 'A'))
            constant2Bool . fst <$> runParser parseConstant "a" `shouldBe` Right True
            runParser parseConstant "+" `shouldBe` Left (MakeSymbolError (InvalidConstant '+'))
        it "parserOperator" $ do
            runParser parseOperator "A" `shouldBe` Left (MakeSymbolError (InvalidOperator 'A'))
            runParser parseOperator "a" `shouldBe` Left (MakeSymbolError (InvalidOperator 'a'))
            operator2Bool . fst <$> runParser parseOperator "+" `shouldBe` Right True

    describe "e2Movesのテスト" $ do
        it "success" $ do
            let angle = pi/2
            let (Right expr) = fst <$> runParser parseExpression "+A"
            let ret = getPoss expr angle (0, 0)
            ret `shouldBe` [(0, 0), (6.123233995736766e-15,100.0)]
        it "success" $ do
            let angle = pi/2
            let (Right expr) = fst <$> runParser parseExpression "+-A"
            let ret = getPoss expr angle (0, 0)
            ret `shouldBe` [(0, 0), (100, 0)]
        -- it "succsess2" $ do
        --     let angle = pi/2
        --     let moves = map (\f -> f angle) . e2Moves . fst <$> runParser parseExpression "+A"
        --     let poss = scanl (\pos f -> f pos) (0, 0) <$> moves
        --     poss `shouldBe` Right  (scanl (\pos f -> f pos) (0, 0) [moveFoward 1 angle])

        -- it "succsess3" $ do
        --     let angle = pi/2
        --     let moves = map (\f -> f angle). e2Moves . fst <$> runParser parseExpression "-A"
        --     let finalPos = scanl (\pos f -> f pos) (0, 0) <$> moves
        --     finalPos `shouldBe` Right (scanl (\pos f -> f pos) (0, 0) [moveFoward (-1) angle])

        -- it "succsess3" $ do
        --     let angle = pi/3
        --     let moves = map (\f -> f angle). e2Moves . fst <$> runParser parseExpression "A + B - - A + B + + - -"
        --     let poss = scanl (\pos f -> f pos) (0, 0) <$> moves
        --     poss `shouldBe` 
        --         Right (
        --             scanl (\pos f -> f pos) (0, 0)
        --                 (map (\f -> f angle) [moveFoward 0, moveFoward 1, moveFoward (-1), moveFoward 0])
        --             )

variable2Bool :: Symbol -> Bool
variable2Bool (Var _) = True
variable2Bool _ = False
constant2Bool :: Symbol -> Bool
constant2Bool (Const _) = True
constant2Bool _ = False
operator2Bool :: Symbol -> Bool
operator2Bool (OP _) = True
operator2Bool _ = False
