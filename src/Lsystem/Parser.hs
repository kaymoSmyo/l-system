module Lsystem.Parser 
    ( Parser
    
    ) where

import Control.Applicative
import Data.Char

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
    \input -> case input of
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

symbol :: String -> Parser String
symbol ss = token (string ss)

isValidChar :: Parser Char
isValidChar = symbolChar '+' <|> symbolChar '-' <|> alphabet

