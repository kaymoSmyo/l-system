module Lsystem.Types
    ( Symbol
    , makeVariable
    , makeConstant
    , makeOP
    ) where

import Data.Char (isUpper, isAlpha)
-- BNF
-- DisplaceRule ::= Symbol -> Expression
-- Expression ::= Symbol (Expression | ε)
-- Symbol ::= Variable | Constant | OP
-- Variable ::= A | B | C | ...
-- Constant ::= a | b | c | ...
-- OP ::= その他の記号

-- displaces = makeDisplacementRules 
--     [ 'a' `displace` "a+b++b-a--aa-b+"
--     , 'b' `displace` "-a+bb++b+a--a-b"
--     ]
-- 最終的にこのように使いたい

-- パースされた結果は[Symbol]として返し、[moveFoward n] 関数のリストになる
-- 関数のリストを作成するときはSymbolに対するパターンマッチをすればできそう
-- 各Charをどの操作に対応させるかはどうする？
-- +/-はそのままにして、一般化したものの特殊例として実装
-- moveFowardは角度(+/-の数と設定された角度)、前回の結果から新しい結果を生成する
-- より一般化した形にも対応したい
-- makeOriginalCorresponds -- Symbolと関数の対応の設定
--     [ ワンステップ進む [Symbol A, Symbol C]
--     , 二倍の長さ進む [Symbol B]
--     ]

-- 
-- Variable は英大文字一文字、Constant 英子文字一文字、OPは記号類一文字として設定
-- Symbol ::= Variable | Constant | OP
-- Variable ::= A | B | C | ...
-- Constant ::= a | b | c | ...
-- OP ::= その他の記号
-- 上の規則に相当する
-- 直接型コンストラクタを使用して、Variable等を作成することはしない
-- Char -> Symbolとなる関数のなかで、英大文字か小文字か、記号かを判定する関数を作成
-- この関数をエクスポートして、外部ではこの関数のみでしか、Variable等を作れないようにする
data Symbol = Variable Char | Constant Char | OP Char
    deriving (Eq, Show)

newtype Expression = Expression [Symbol]

-- 具体的なエラー型の定義
data SymbolError =
    InvalidVariable Char |
    InvalidConstant Char |
    InvalidOperator Char
    deriving (Eq, Show)

-- スマートコンストラクタ
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
    | not (isAlpha c) = Right (OP c)
    | otherwise = Left (InvalidOperator c)
