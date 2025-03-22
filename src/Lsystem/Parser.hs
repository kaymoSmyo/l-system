{-# LANGUAGE LambdaCase #-}

module Lsystem.Parser
    (
    ) where

import Control.Applicative
import Data.Char
import Lsystem.Types

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    -- fmap :: (a -> b) ->  Parser a -> Parser b
    fmap f p = P (
        \input ->
            case runParser p input of
                [] -> []
                [(x, out)] -> [(f x, out)]
        )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\input -> [(a, input)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pa2b <*> pa = P (
        \input -> case runParser pa2b input of
            [] -> []
            [(a2b, out)] -> runParser (fmap a2b pa) out
        )
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= a2pb = P (
        \input -> case runParser pa input of
            [] -> []
            [(a, out)] -> runParser (a2pb a) out
        )
instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (
        \input -> case runParser p input of
            [] -> runParser q input
            [(a, out)] -> [(a, out)]
        )

runParser :: Parser a -> String -> [(a, String)]
runParser (P p) = p

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

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

-- Parser Expression => P (String -> [(Expression, String)])
-- ここで[(Expression, String)]となっているのは空リストで失敗を表すから
-- Expression == [Symbol]なので、パーサ内でmakeVariable :: Char -> Either SymbolError Symbolを使う必要
-- 一文字とって、makeVariableする
parseVariabe :: Parser Symbol
parseVariabe = token (P f)
    where
        f (x:xs) = case makeVariable x of
            Left _ -> []
            Right vx -> [(vx, xs)]
        f [] = []

parseConstant :: Parser Symbol
parseConstant = token (P f)
    where
        f (x:xs) = case makeConstant x of
            Left _ -> []
            Right cx -> [(cx, xs)]
        f [] = []

parseOperator :: Parser Symbol
parseOperator = token (P f)
    where
        f (x:xs) = case makeOperator x of
            Left _ -> []
            Right ox -> [(ox, xs)]
        f [] = []

parseSymbol :: Parser Symbol
parseSymbol = token (parseVariabe <|> parseConstant <|> parseOperator)

parseExpression :: Parser Expression
parseExpression = do
    s <- parseSymbol
    ((s : ) <$> parseExpression) <|> pure [s]

-- parseDisplaceRule :: Parser DisplaceRule
-- parseDisplaceRule = 