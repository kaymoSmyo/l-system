{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module Lib
    ( moveFoward
    , apply
    , parseExpr
    , nDisplacement
    , getSousa
    , DisplaceRule
    , Pos
    , Expr
    , parseDisplaceRule
    ) where
import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as M

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
foward = (100, 0)

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

apply :: [Float -> Pos -> Pos] -> Float -> Pos -> [Pos]
apply fs angle initial = scanl (\pos f -> f pos) initial fs'
    where
        fs' = map (\f -> f angle) fs

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

-- パース(空白の除去しかしていない)
-- 変数はアルファベット一文字であると変更
expr :: Parser Char
expr = symbolChar '+' <|> symbolChar '-' <|> alphabet

parseExpr :: String -> Expr
parseExpr [] = []
parseExpr ss = s : parseExpr out
    where
        (s, out) = head $ parse expr ss

-- 置換方法は辞書型に入れる
type DisplaceRule = M.Map Char Expr
displacement :: DisplaceRule -> Expr -> Expr
displacement _ [] = []
displacement dr (e:es) = case M.lookup e dr of
    Just t -> t ++ displacement dr es
    Nothing -> e : displacement dr es

-- 任意回数の置換
nDisplacement :: DisplaceRule -> Int -> Expr -> Expr
nDisplacement _ 0 es = es
nDisplacement dr n es = nDisplacement dr (n-1) (displacement dr es)

-- 関数への変換
getSousa :: Expr -> [Float -> Pos -> Pos]
getSousa e = expr2Func e 0
    where
        expr2Func :: Expr -> Int -> [Float -> Pos -> Pos]
        expr2Func [] _ = []
        expr2Func (s:ss) acc =
            let (newacc, lf)
                    | s == '+' = (acc+1, [])
                    | s == '-' = (acc-1, [])
                    | isAlpha s = (acc, [moveFoward acc])
                    | otherwise = error "不正な文字種が入力されました"
            in lf ++ expr2Func ss newacc

-- "A->A-B"
-- (A, A-B)に分割
-- alphabet <* symbol "->"
parseDisplaceRule :: String -> (Char, Expr)
parseDisplaceRule ss = case parse (alphabet <* symbol "->") ss of
    [(c, out)] -> (c, parseExpr out)
    [] -> error "不正な記述の置換規則"
    _  -> error "不正な記述の置換規則"

space :: Parser ()
space = many (sat isSpace) >> return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

alphabet :: Parser Char
alphabet = token (sat isAlpha)

symbolChar :: Char -> Parser Char
symbolChar c = token (char c)

string :: String -> Parser String
string [] = pure []
string (x:xs) = char x *> string xs *> pure (x:xs)

symbol ::String -> Parser String
symbol ss = token (string ss)
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

sat :: (Char -> Bool) -> Parser Char
sat predicate = do
    x <- item
    if predicate x then
        pure x
    else
        empty

char :: Char -> Parser Char
char x = sat (==x)
