module Lsystem 
    ( displace
    , getPositions
    , makeDisplacementRules
    ) where

-- A -> A - B という式は
-- 'A' `displace` "A-B"
-- と表す

-- dispace :: 一文字の変数 -> 適切な記号が使われた式 -> DisplacRule
-- char -> 一文字の変数 String -> 適切な記号が使われた式
-- 上の二つがあればok
-- BNF
-- DisplaceRule ::= Identifer -> Expression
-- Expression ::= Term 
-- Term ::= Identifer+ | OP+
-- Identifer ::= "A" | "B" | "C" ...
-- OP ::= + | -

import Lsystem.Core
displace :: Char -> String -> (Identifer, Expression)
displace c ss = (makeIdent c , makeExpr ss)


-- dragonCurve = getPositions (0, 0) 90 dragonDisplaces (makeExpr "a") 12

getPositions :: Pos -> Expression -> Double -> DisplaceRules -> Int ->  [(Double, Double)]
getPositions start expr angle rules n = scanl (\pos f -> f pos) start moves 
    where
        moves = map (\f -> f angle) (e2Moves $ nExpansion expr rules n)
