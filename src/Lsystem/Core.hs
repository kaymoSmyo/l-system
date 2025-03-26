module Lsystem.Core
    ( Pos
    , nExpansion
    , e2Moves
    , foward
    , moveFoward
    , getPoss
    , makeDRs
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
moveFoward :: Int -> Double -> Pos -> Pos
moveFoward acc angle cxy = (x', y')
    where
        t = angle * fromIntegral acc
        x = cos t * foward
        y = sin t * foward
        (x', y') = (x, y) `addPos` cxy

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

-- 最終的な処理、座標を計算する
getPoss :: DRs -> Angle -> Int -> String -> Pos -> [Pos]
getPoss drs angle n s start = poss
    where
        expr = fst . head $ runParser parseExpression s
        poss = scanl (\p f -> f p) start moves
        moves = map (\f -> f angle) $ e2Moves $ nExpansion expr drs n

makeDRs :: [String] -> DRs
makeDRs ss = MS.fromList rules
    where
        rules = do
            s <- ss
            case runParser parseDisplaceRule s of
                [(dr, _)] -> [dr] -- 最初の結果のみ使用
                _ -> error "faild parse"        -- パース失敗
