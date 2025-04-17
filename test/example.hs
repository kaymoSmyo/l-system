module Main (main) where

import Diagrams.Prelude
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

lsyetem :: Lsystem.Expression
lsyetem = case evalLsystem "A" ["A -> A - B", "B -> A + B"] 10 of
    Right expr -> expr
    Left err -> error (show err)

result :: [Lsystem.Pos]
result = execLsystem lsyetem (pi/2) (0, 0)