module Lsystem 
    ( displace
    , makeExpr
    , DisplaceRules
    , makeDisplacementRules
    , getPositions
    ) where


import Lsystem.Core
displace :: Char -> String -> (Identifer, Expression)
displace c ss = (makeIdent c , makeExpr ss)


-- dragonCurve = getPositions (0, 0) 90 dragonDisplaces (makeExpr "a") 12

getPositions :: Pos -> Expression -> Double -> DisplaceRules -> Int ->  [(Double, Double)]
getPositions start expr angle rules n = scanl (\pos f -> f pos) start moves 
    where
        moves = map (\f -> f angle) (e2Moves $ nExpansion expr rules n)
