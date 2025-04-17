module Lsystem.EvalExpr
    ( Pos
    , nExpansion
    , foward
    , moveFoward
    , getPoss
    , exprToMoves
    , Angle
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


type Pos = (Double, Double)
type Angle = Double

addPos :: Pos -> Pos -> Pos
(x, y) `addPos` (a, b) = (x + a, y + b)

-- 前進の移動量
foward :: Double
foward = 100

-- 1ステップ前進
moveFoward :: Angle -> Pos -> Pos
moveFoward angle cxy = (x', y')
    where
        x = cos angle * foward
        y = sin angle * foward
        (x', y') = (x, y) `addPos` cxy

-- 与えられた式をmoveFowardのリストに変換
exprToMoves :: Expression -> Angle -> [Pos -> Pos]
exprToMoves expr angle = f expr 0
    where
        -- thetaは現在の角度を表す
        f :: Expression -> Angle -> [Pos -> Pos]
        f [] _ = []
        f (OP s : ss) theta
            | s == '+' = f ss (theta + angle)
            | s == '-' = f ss (theta - angle)
            | otherwise = f ss theta
        f (_ : ss) theta = moveFoward theta : f ss theta

-- 最終的な処理、座標を計算する
getPoss :: Expression -> Angle -> Pos -> [Pos]
getPoss expr angle start = 
    let moves = exprToMoves expr angle
        poss = scanl (\p f -> f p) start moves
    in poss
