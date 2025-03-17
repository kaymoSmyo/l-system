module Main (main) where
import Lib
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = print "A"

-- draw :: String -> Float -> DisplaceRule -> Int -> Diagram B
-- draw ss angle dr n = 
--     fromVertices (map p2 points)
--       # stroke
--       # lw medium
--       # frame 0.1
--     where
--         points = Lib.apply sousa angle (0, 0)
--         sousa = getSousa $ nDisplacement dr n $ parseExpr ss