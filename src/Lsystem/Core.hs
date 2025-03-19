module Lsystem.Core
    ( Identifer
    , makeIdent
    , Expression
    , makeExpr
    , DisplaceRules
    , makeDisplacementRules
    ) where

import Data.Char (isAlpha)
import qualified Data.Map.Strict as MS


newtype Identifer = Identifer Char
instance Eq Identifer where
    (Identifer x) == (Identifer y) = x == y
instance Ord Identifer where
    (Identifer x) <= (Identifer y) = x <= y
    
makeIdent :: Char -> Identifer
makeIdent c
    | isAlpha c = Identifer c
    | otherwise = error "変数はアルファベット一文字のみを設定できます"

newtype Expression = Expression String
makeExpr :: String -> Expression
makeExpr ss
    | all (\c -> isAlpha c || c == '+' || c == '-') ss = Expression ss
    | otherwise = error "無効な記号が式に含まれています。式中に使える記号はアルファベットと+/-のみです"

newtype DisplaceRules = DR (MS.Map Identifer Expression)
-- dragonTikan = DisPlacementRule ['a' `displace` "a - b",  'b' `displace` "a + b" ]
makeDisplacementRules :: [(Identifer, Expression)] -> DisplaceRules
makeDisplacementRules xs = DR (MS.fromList xs)