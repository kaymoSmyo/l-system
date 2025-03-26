module Lsystem 
    ( 
    ) where
import Lsystem.Parser
import Lsystem.Core
import Lsystem.Types
import qualified Data.Map.Strict as MS


makeDRs :: [String] -> DRs
makeDRs ss = MS.fromList rules
    where
        rules = do
            s <- ss
            case runParser parseDisplaceRule s of
                [(dr, _)] -> [dr] -- 最初の結果のみ使用
                _ -> error "faild parse"        -- パース失敗
