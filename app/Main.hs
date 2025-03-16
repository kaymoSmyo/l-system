module Main (main) where

import Lib (moveFoward, Pos)

main :: IO ()
main = print kochresult

-- 式をパースするときは最終的にこのリストを返す
-- koch :: [Pos -> Pos]
sousa :: Float -> [Pos -> Pos]
sousa angle = map (\f -> f angle)
    [ moveFoward 0
    , moveFoward 1
    , moveFoward (-1)
    , moveFoward 0
    ]


apply :: [Pos -> Pos] -> Pos -> [Pos]
-- apply [] _ = []
-- apply (f:fs) xy = nxy : apply fs nxy
--     where
--         nxy = f xy
apply fs initial = scanl (\pos f -> f pos) initial fs

-- b = pos a = pos -> pos
-- (pos -> (pos -> pos) -> pos) -> pos -> [pos -> pos] -> [pos]
kochresult :: [(Float, Float)]
kochresult = apply (sousa (pi/3)) (0, 0)
