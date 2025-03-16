module Main (main) where

import Lib (moveFoward, Pos)

main :: IO ()
main = print 0

-- 式をパースするときは最終的にこのリストを返す
-- koch :: [Pos -> Pos]
sousa :: [ Float -> Pos -> Pos]
sousa =
    [ moveFoward 0
    , moveFoward 1
    , moveFoward (-1)
    , moveFoward 0
    ]
