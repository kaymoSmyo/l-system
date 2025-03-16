{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module Lib
    ( moveFoward
    , Pos
    ) where

import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP (many1)

-- 方針
-- A : A + A - - A + Aの形で辞書型を作る
-- 与えられた式の展開 String -> String
-- | 辞書型にあるなら、展開
-- | ないなら、そのまま
-- 文字列を関数の連続へとパースする
type Pos = (Float, Float)

addPos :: Pos -> Pos -> Pos
(x, y) `addPos` (a, b) = (x + a, y + b)

-- 前進の移動量は1とする
foward :: Pos
foward = (1, 0)

-- 回転量は acc * thetaで表すことができる
-- accはパースするときに、+がでたら+1、-がでたら-1する
-- (1,0)のベクトルを回転させ、今いるベクトルの終点に移動させることで、次のベクトルの終点の座標を得る
moveFoward :: Int -> Float -> Pos -> Pos
moveFoward acc angle cxy = (x', y')
    where
        t = angle * fromIntegral acc
        x = cos t * fst foward
        y = sin t * fst foward
        (x', y') = (x, y) `addPos` cxy

-- "A+A"のような文字列を置換する
-- 例 コッホ曲線
-- Aの置換方法: A -> A + A - - A + A
-- 回転する角度: 60
-- 再帰する回数: 1
-- 与えられる式: A
-- A を一回置換し、A + A - - A + A として実行

-- 手順
-- 置換と式の空白の除去 |> 置換方法と式の取得 |> 関数への変換

-- "A+A"
type Expr = [Char]

-- パース
-- 変数はアルファベット一文字であると変更
expr :: Parser Char
expr = symbolChar '+' <|> symbolChar '-' <|> alphabet

parseExpr :: String -> Expr
parseExpr [] = []
parseExpr ss = s : parseExpr out
    where
        (s, out) = head $ parse expr ss

-- 置換方法は辞書型に入れる
type DisplaceRule = M.Map Char String
displacement :: DisplaceRule -> Expr -> Expr
displacement _ [] = []
displacement dr (s:ss) = case M.lookup s dr of
    Just t -> t ++ displacement dr ss
    Nothing -> s : displacement dr ss

-- 関数への変換
expr2Func :: Expr -> Int -> [Float -> Pos -> Pos]
expr2Func [] _ = []
expr2Func (s:ss) acc =
    let newacc
            | s == '+' = acc+1
            | s == '-' = acc-1
            | isAlpha s = acc
            | otherwise = error "不正な文字種が入力されました"
    in moveFoward newacc : expr2Func ss newacc

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    -- fmap :: (a -> b) ->  Parser a -> Parser b
    fmap f p = P (
        \input ->
            case parse p input of
                [] -> []
                [(x, out)] -> [(f x, out)]
                _ -> []
        )
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\input -> [(a, input)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pa2b <*> pa = P (
        \input -> case parse pa2b input of
            [] -> []
            [(a2b, out)] -> parse (fmap a2b pa) out
            _ -> []
        )
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= a2pb = P (
        \input -> case parse pa input of
            [] -> []
            [(a, out)] -> parse (a2pb a) out
            _ -> []
        )
instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (
        \input -> case parse p input of
            [] -> parse q input
            [(a, out)] -> [(a, out)]
            _ -> []
        )

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P (
    \case
        [] -> []
        (x:xs) -> [(x, xs)]
    )

three :: Parser (Char, Char)
three = g <$> item <*> item <*> item
    where
        g x y z = (x, z)

sat :: (Char -> Bool) -> Parser Char
sat predicate = do
    x <- item
    if predicate x then
        pure x
    else
        empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)


string :: String -> Parser String
string [] = return []
string (x:xs) = char x >> string xs >> pure (x:xs)

ident :: Parser String
ident = do
    -- x <- lower
    x <- some lower
    xs <- many alphanum
    return (x ++ xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = many (sat isSpace) >> return ()

int :: Parser Int
int = do
    _ <- char '-'
    n <- nat
    return (-n)
    <|>
    nat


token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

alphabet :: Parser Char
alphabet = token (sat isAlpha)
symbolChar :: Char -> Parser Char
symbolChar c = token (char c)

interger :: Parser Int
interger = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

natural :: Parser Int
natural = token int