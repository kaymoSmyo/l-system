module Main (main) where

import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo

import Lsystem
-- 手順
-- 置換と式の空白の除去 |> 置換方法と式の取得 |> 関数への変換

main :: IO ()
main = renderCairo "output.svg" (dims (V2 800 800)) (drawPolyline result)
drawPolyline :: [(Double, Double)] -> Diagram B
drawPolyline points =
    fromVertices (map (p2 . toDouble) points)
        # strokeLine
        # lw ultraThin
        # lc Diagrams.Prelude.black
        # bgFrame 100 lightgray
        # frame 1
    where
        toDouble (x, y) = (realToFrac x :: Double, realToFrac y :: Double)

displaces :: DisplaceRules
displaces = makeDisplacementRules 
    [ 'a' `displace` "a+b++b-a--aa-b+"
    , 'b' `displace` "-a+bb++b+a--a-b"
    ]
result :: [(Double, Double)]
result = 
    getPositions 
        (0, 0) 
        (makeExpr "a") 
        (pi/3) 
        displaces 
        5