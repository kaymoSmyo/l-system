module Lib
    ( someFunc
    ) where
import qualified Data.Map.Strict

-- L-SYSTEMのDSLをつくる
-- L-SYSTEMの文字列を受けっとてパースし、実行する
-- A, B C..など、アルファベット大文字小文字を自由に追加、再帰の回数の指定ができるように
-- アルファベットは1ステップ前進、+は向きを左回転、-は向きを右回転

-- 例 コッホ曲線
-- Aの置換方法: A -> A + A - - A + A
-- 回転する角度: 60
-- 与えられる式: A
-- 再帰する回数: 1
-- A を一回展開し、A + A - - A + A として実行

-- 方針
-- A : A + A - - A + Aの形で辞書型を作る
-- 与えられた式の展開 String -> String
-- | 辞書型にあるなら、展開
-- | ないなら、そのまま
-- 文字列を関数の連続へとパースする


type Pos = (Float, Float)


-- 前進の移動量は1とする
foward :: Pos
foward = (1, 0)

-- 回転の角度
theta :: Float
theta = pi / 3

addPos :: Pos -> Pos -> Pos
(x, y) `addPos` (a, b) = (x + a, y + b)

-- 回転量は acc * thetaで表すことができる
-- accはパースするときに、+がでたら+1、-がでたら-1する
-- (1,0)のベクトルを回転させ、今いるベクトルの終点に移動させることで、次のベクトルの終点の座標を得る
moveFoward :: Int -> Pos -> Pos
moveFoward acc cxy = (x', y')
    where
        t = theta * fromIntegral acc
        x = cos t * fst foward
        y = sin t * fst foward 
        (x', y') = (x, y) `addPos` cxy

apply :: [Pos -> Pos] -> Pos -> [Pos]
apply [] _ = []
apply (f:fs) xy = nxy : apply fs nxy 
    where
        nxy = f xy

-- 式をパースするときは最終的にこのリストを返す
koch :: [Pos -> Pos]
koch = [moveFoward 0, moveFoward 1, moveFoward (-1), moveFoward 0]

kochresult :: [(Float, Float)]
kochresult = (0, 0) : apply koch (0, 0)

someFunc :: IO ()
someFunc = putStrLn "someFunc"