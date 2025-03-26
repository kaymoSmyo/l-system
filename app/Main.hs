module Main (main) where

import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo
import Lsystem

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

drs = makeDRs ["A -> A - B", "B -> A + B"]
result :: [(Double, Double)]
result = getPoss drs (pi/2) 12 "A" (0, 0)