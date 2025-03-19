module Lsystem.Core 
    ( Identifer
    , makeIdent
    , Expression
    , makeExpr
    ) where
        
import Data.Char (isAlpha)



newtype Identifer = Identifer Char
makeIdent :: Char -> Identifer
makeIdent c
    | isAlpha c = Identifer c
    | otherwise = error "変数はアルファベット一文字のみを設定できます"

newtype Expression = Expression String
makeExpr :: String -> Expression
makeExpr ss
    | all (\c -> isAlpha c || c == '+' || c == '-') ss = Expression ss
    | otherwise = error "無効な記号が式に含まれています。式中に使える記号はアルファベットと+/-のみです"