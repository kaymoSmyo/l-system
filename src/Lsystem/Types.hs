{-# LANGUAGE PatternSynonyms #-}
module Lsystem.Types
    ( Symbol
    , SymbolError(..)
    , makeVariable
    , makeConstant
    , makeOP
    , pattern Var
    , pattern Const
    , pattern OP
    ) where

import Data.Char (isUpper, isAlpha)
-- BNF
-- DisplaceRule ::= Symbol -> Expression
-- Expression ::= Symbol (Expression | ε)
-- Symbol ::= Variable | Constant | OP
-- Variable ::= A | B | C | ...
-- Constant ::= a | b | c | ...
-- Operator ::= その他の記号
-- Variable は英大文字一文字、Constant 英子文字一文字、Operatorは記号類一文字として設定

-- displaces = makeDisplacementRules 
--     [ 'a' `displace` "a+b++b-a--aa-b+"
--     , 'b' `displace` "-a+bb++b+a--a-b"
--     ]
-- 最終的にこのように使いたい

-- パースされた結果は[Symbol]として返し、[moveFoward n] 関数のリストになる
-- 関数のリストを作成するときはSymbolに対するパターンマッチをすればできそう
--     コンストラクタをエクスポートさせたくないのにどうする？
-- 各Charをどの操作に対応させるかはどうする？
-- +/-はそのままにして、一般化したものの特殊例として実装

-- moveFowardは角度(+/-の数と設定された角度)、前回の結果から新しい結果を生成する
-- より一般化した形にも対応したい
-- makeOriginalCorresponds -- Symbolと関数の対応の設定
--     [ ワンステップ進む [Symbol A, Symbol C]
--     , 二倍の長さ進む [Symbol B]
--     ]

-- Symbol ::= Variable | Constant | Operator
-- 直接、型コンストラクタを使用して、Variable等を作成することはしない
data Symbol = Variable Char | Constant Char | Operator Char
    deriving (Eq, Show)


-- スマートコンストラクタ用のエラー型の定義
data SymbolError =
    InvalidVariable Char |
    InvalidConstant Char |
    InvalidOperator Char
    deriving (Eq, Show)

-- スマートコンストラクタ
-- この関数をエクスポートして、外部ではこの関数のみでしか、Variable等を作れないようにする
makeVariable :: Char -> Either SymbolError Symbol
makeVariable c 
    | isUpper c = Right (Variable c)
    | otherwise = Left (InvalidVariable c)

makeConstant :: Char -> Either SymbolError Symbol
makeConstant c
    | not (isUpper c) = Right (Constant c)
    | otherwise = Left (InvalidConstant c)

makeOP :: Char -> Either SymbolError Symbol
makeOP c
    | not (isAlpha c) = Right (Operator c)
    | otherwise = Left (InvalidOperator c)

{-# COMPLETE Var, Const, OP #-}
-- 外部の関数でのパターンマッチ用
-- Symbolのコンストラクタをエクスポートしないため、このパターンシノニムをエクスポートする
pattern Var :: Char -> Symbol
pattern Var c <- Variable c

pattern Const :: Char -> Symbol
pattern Const c <- Constant c

pattern OP :: Char -> Symbol
pattern OP c <- Operator c

-- 今後作るパーサで返すもの
newtype Expression = Expression [Symbol]