module Lsystem.Core
    ( Pos
    , nExpansion
    , e2Moves
    ) where

import Data.Char (isAlpha, isSpace)
import qualified Data.Map.Strict as MS
import Lsystem.Types


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

-- 与えられた式を与えられた置換規則したがって、n回置換していく
-- nExpansion :: Expression -> DisplaceRules -> Int -> Expression
-- nExpansion expr _ 0 = expr
-- nExpansion (Expression es) (DR dr) n = nExpansion (makeExpr nss) (DR dr) (n-1)
--     where
--         expansion :: String -> MS.Map Identifer Expression -> String
--         expansion [] _ = []
--         expansion (s:ss) dr' = 
--             case MS.lookup (Identifer s) dr' of
--                 Just (Expression expr) -> expr ++ expansion ss dr'
--                 Nothing -> s : expansion ss dr'
--         nss = expansion es dr

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
