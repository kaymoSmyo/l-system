module Lsystem.Core
    ( Pos
    , nExpansion
    , e2Moves
    , foward
    , moveFoward
    ) where

import qualified Data.Map.Strict as MS
import Lsystem.Types

-- 与えられた式を与えられた置換規則したがって、n回置換していく
nExpansion :: Expression -> DRs -> Int -> Expression
nExpansion expr _ 0 = expr
nExpansion [] _ _ = []
nExpansion expr drs n = nExpansion expr' drs (n-1)
    where 
        expr' = expansion expr drs
        expansion :: Expression -> DRs -> Expression
        expansion [] _ = []
        expansion (s@(Const _): ss) tmpDRs = s : expansion ss tmpDRs
        expansion (s@(OP _) : ss) tmpDRs = s : expansion ss tmpDRs
        expansion (s: ss) tmpDRs =
                case MS.lookup s tmpDRs of
                    Just t -> t ++ expansion ss tmpDRs
                    Nothing -> s : expansion ss tmpDRs

-- 与えられた式をmoveFowardのリストに変換
e2Moves :: Expression -> [Double -> Pos -> Pos]
e2Moves expr = f expr 0
    where
        f :: Expression -> Int -> [Double -> Pos -> Pos]
        f [] _ = []
        f (OP s : ss) n 
            | s == '+' = f ss (n+1)
            | s == '-' = f ss (n-1)
            | otherwise = f ss n
        f (_ : ss) n = moveFoward n : f ss n

type Pos = (Double, Double)

addPos :: Pos -> Pos -> Pos
(x, y) `addPos` (a, b) = (x + a, y + b)

-- 前進の移動量
foward :: Double
foward = 100

-- 1ステップ前進
moveFoward :: Int -> Double -> Pos -> Pos
moveFoward acc angle cxy = (x', y')
    where
        t = angle * fromIntegral acc
        x = cos t * foward
        y = sin t * foward
        (x', y') = (x, y) `addPos` cxy
