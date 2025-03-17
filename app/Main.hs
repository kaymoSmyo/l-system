module Main (main) where
-- import Graphics.Gloss
import Lib
    ( apply
    , getSousa
    , nDisplacement
    , parseExpr
    , DisplaceRule
    , Pos
    , parseDisplaceRule
    )
import qualified Data.Map.Strict as M
import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo
-- 手順
-- 置換と式の空白の除去 |> 置換方法と式の取得 |> 関数への変換

main :: IO ()
main = renderCairo "output.svg" (dims (V2 800 800)) (drawPolyline dragonResult)
drawPolyline :: [(Float, Float)] -> Diagram B
drawPolyline points =
    fromVertices (map (p2 . toDouble) points)
        # strokeLine
        # lw ultraThin
        # lc Diagrams.Prelude.black
        # bgFrame 100 lightgray
        # frame 1
    where
        toDouble (x, y) = (realToFrac x :: Double, realToFrac y :: Double)
dragonTikan = M.fromList [('a', "a-b"), ('b', "a+b")]
dragonSousa = getSousa $ nDisplacement dragonTikan 11 $ parseExpr "a"
dragonResult = Lib.apply dragonSousa (pi/2) (0, 0)
-- main = do
--     putStrLn "置換規則の数を入力してください"
--     n <- getLine

--     putStrLn "置換規則を入力してください(例 A -> A + B)"
--     es <- nGetLine (read n)
--     let rules = M.fromList (map parseDisplaceRule es)

--     putStrLn "回転させる角度を入力してください"
--     angle <- getLine

--     putStrLn "最初の式を入力してください"
--     e <- getLine

--     putStrLn "置換する回数を入力してください"
--     times <- getLine

--     draw (getPoss e (2*pi*(read angle ::Float)/360) rules (read times))

-- draw :: [Pos] -> IO ()
-- draw ps = display window background picture
--     where
--         -- ウィンドウの設定：タイトル、サイズ（400x400）、位置（10,10）を指定
--         window = InWindow "Line Drawing" (1000, 1000) (10, 10)
--         -- 背景色を白に設定
--         background = white
--         -- 2点を結ぶ線を描画：(100,100)から(300,300)まで
--         picture = pictures
--             [ color black $ line ps]

getPoss :: String -> Float -> DisplaceRule -> Int -> [Pos]
getPoss e angle rules n = Lib.apply sousa angle (0, 0)
    where
        sousa = getSousa $ nDisplacement rules n $ parseExpr e

nGetLine :: Int -> IO [String]
nGetLine 0 = pure []
nGetLine n = do
    ss <- getLine
    sss <- nGetLine (n-1)
    pure (ss : sss)
