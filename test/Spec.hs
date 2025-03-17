import Lib
    ( getSousa
    , apply
    , parseExpr
    , nDisplacement
    )
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Graphics.Gloss
import qualified Data.Map.Strict as M

main :: IO ()
-- main = do
--     draw dragonResult
main = mainWith (drawPolyline dragonResult)
-- | 頂点リストを結ぶ線を描画する
drawPolyline :: [(Float, Float)] -> Diagram B
drawPolyline points =
    fromVertices (map (p2 . (\(x, y) -> (realToFrac x, realToFrac y))) points)
        # strokeLine
        # lw thick
        # lc Diagrams.Prelude.blue

draw :: [Graphics.Gloss.Point] -> IO ()
draw ps = display window background picture
    where
        -- ウィンドウの設定：タイトル、サイズ（400x400）、位置（10,10）を指定
        window = InWindow "Line Drawing" (1000, 1000) (10, 10)
        -- 背景色を白に設定
        background = Graphics.Gloss.white
        -- 2点を結ぶ線を描画：(100,100)から(300,300)まで
        picture = pictures
            [ color Graphics.Gloss.black $ line ps]
simpleTikan = M.fromList [('A', parseExpr "A+A")]
simpleSousa = getSousa $ nDisplacement simpleTikan 1  $ parseExpr "A+A"

kochtikan = M.fromList [('A', parseExpr "A + A - - A + A")]
kochSousa = getSousa $ nDisplacement kochtikan 3  $ parseExpr "A"
kochResult = Lib.apply kochSousa (pi/3) (0, 0)

dragonTikan = M.fromList [('a', "a-b"), ('b', "a+b")]
dragonSousa = getSousa $ nDisplacement dragonTikan 15 $ parseExpr "a"
dragonResult = Lib.apply dragonSousa (pi/2) (0, 0)