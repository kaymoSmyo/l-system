import Lib
    ( getSousa
    , apply
    , parseExpr
    , nDisplacement
    )
import qualified Data.Map.Strict as M
main :: IO ()
main = do
    print kochResult

simpleTikan = M.fromList [('A', parseExpr "A+A")]
simpleSousa = getSousa $ nDisplacement simpleTikan 1  $ parseExpr "A+A"

kochtikan = M.fromList [('A', parseExpr "A + A - - A + A")]
kochSousa = getSousa $ nDisplacement kochtikan 3  $ parseExpr "A"
kochResult = apply kochSousa (pi/3) (0, 0)