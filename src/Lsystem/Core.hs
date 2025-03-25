module Lsystem.Core
    ( Pos
    , nExpansion
    ) where

import qualified Data.Map.Strict as MS
import Lsystem.Types
import Lsystem.Parser

-- 与えられた式を与えられた置換規則したがって、n回置換していく
nExpansion :: Expression -> DRs -> Int -> Expression
nExpansion expr _ 0 = expr
nExpansion [] _ _ = []
nExpansion expr drs n = nExpansion expr' drs (n-1)
    where 
        expr' = expansion expr drs
        expansion :: Expression -> DRs -> Expression
        expansion [] _ = []
        expansion (s: ss) tmpDRs =
                case MS.lookup s tmpDRs of
                    Just t -> t ++ expansion ss tmpDRs
                    Nothing -> s : expansion ss tmpDRs

type Pos = (Double, Double)

addPos :: Pos -> Pos -> Pos
(x, y) `addPos` (a, b) = (x + a, y + b)

-- 前進の移動量
foward :: Pos
foward = (100, 0)

-- 1ステップ前進
moveFoward :: Int -> Double -> Pos -> Pos
moveFoward acc angle cxy = (x', y')
    where
        t = angle * fromIntegral acc
        x = cos t * fst foward
        y = sin t * fst foward
        (x', y') = (x, y) `addPos` cxy

 
-- 与えられた式から、moveFowardのリストを作成
-- e2Moves :: Expression -> [Double -> Pos -> Pos]
-- e2Moves (Expression es) = e2Move es 0
--     where
--         e2Move :: String -> Int -> [Double -> Pos -> Pos]
--         e2Move [] _ = []
--         e2Move (s:ss) acc = 
--             let (newacc, lf)
--                     | s == '+' = (acc+1, [])
--                     | s == '-' = (acc-1, [])
--                     | isAlpha s = (acc, [moveFoward acc])
--                     | otherwise = error "不正な文字種が入力されました"
--             in lf ++ e2Move ss newacc
