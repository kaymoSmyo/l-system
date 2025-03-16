import Lib
    ( displacement
    , getSousa
    , apply
    , parseExpr
    )
import qualified Data.Map.Strict as M
main :: IO ()
main = do
    print kochResult

simpleTikan = M.fromList [('A', parseExpr "A+A")]
simpleSousa = getSousa $ displacement simpleTikan $ parseExpr "A+A"

kochtikan = M.fromList [('A', parseExpr "A + A - - A + A")]
kochSousa = getSousa $ displacement kochtikan $ parseExpr "A"
kochResult = apply kochSousa (pi/3) (0, 0)