module Main (main) where
import Lib
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = mainWith myCircle

myCircle :: Diagram B
myCircle = circle 1