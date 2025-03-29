{-# LANGUAGE LambdaCase #-}
module Lsystem.Parser
    ( parseVariabe
    , parseConstant
    , parseOperator
    , parseExpression
    , parseDisplaceRule
    , runParser
    ) where

import Control.Applicative
import Data.Char
import Lsystem.Types

newtype Parser a = P (String -> Either String (a, String))

instance Functor Parser where
    -- fmap :: (a -> b) ->  Parser a -> Parser b
    fmap f p = P (
        \input ->
            case runParser p input of
                Left s -> Left s
                Right (a, out) -> Right (f a, out)
        )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\input -> Right (a, input))
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pa2b <*> pa = P (
        \input -> case runParser pa2b input of
            Left s -> Left s
            Right (a2b, out) -> runParser (fmap a2b pa) out
        )
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= a2pb = P (
        \input -> case runParser pa input of
            Left s -> Left s
            Right (a, out) -> runParser (a2pb a) out
        )
instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const (Left ""))
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (
        \input -> case runParser p input of
            Left _ -> runParser q input
            Right (a, out) -> Right (a, out)
        )

runParser :: Parser a -> String -> Either String (a, String)
runParser (P p) = p

item :: Parser Char
item = P (
    \case
        [] -> Left "input: Empty String"
        (x:xs) -> Right (x, xs)
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
    _ <- many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    _ <- char x
    _ <- string xs
    return (x:xs)

parseString :: String -> Parser String
parseString ss = token (string ss)

smbl :: (Char -> Either SymbolError Symbol) -> String -> Either String (Symbol, String)
smbl _ [] = Left "input: Empty String"
smbl f (x:xs) = case f x of
    Left err -> Left (show err) 
    Right s -> Right (s, xs)

-- Parser Expression => P (String -> [(Expression, String)])
-- ここで[(Expression, String)]となっているのは空リストで失敗を表すから
-- Expression == [Symbol]なので、パーサ内でmakeVariable :: Char -> Either SymbolError Symbolを使う必要
-- 一文字とって、makeVariableする
parseVariabe :: Parser Symbol
parseVariabe = token $ P (smbl makeVariable)

parseConstant :: Parser Symbol
parseConstant = token $ P (smbl makeConstant)
    
parseOperator :: Parser Symbol
parseOperator = token $ P (smbl makeOperator)

parseSymbol :: Parser Symbol
parseSymbol = token (parseVariabe <|> parseConstant <|> parseOperator)

parseExpression :: Parser Expression
parseExpression = do
    s <- parseSymbol
    ((s : ) <$> parseExpression) <|> pure [s]

parseDisplaceRule :: Parser DisplaceRule
parseDisplaceRule = (,) <$> parseVariabe <*> (parseString "->" *> parseExpression)
