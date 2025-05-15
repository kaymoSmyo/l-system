module Lsystem.InOut
    ( evalLsystem
    , execLsystem
    ) where

import Lsystem.EvalExpr
import Lsystem.Parser
import Lsystem.Types


evalLsystem :: String -> [String] -> Int -> Either ParseError Expression
evalLsystem a ss n = do
    axiom <- fst <$> runParser parseExpression a
    rules <- mapM parseRule ss
    let drs = fromList rules
    pure $ nExpansion axiom drs n
    where
        parseRule :: String -> Either ParseError DisplaceRule
        parseRule s = fst <$> runParser parseDisplaceRule s

execLsystem :: Expression -> Angle -> Pos -> [Pos]
execLsystem = getPoss 