module Lsytem.Types 
    (

    ) where

-- BNF
-- DisplaceRule ::= Symbol -> Expression
-- Expression ::= Symbol (Expression | ε)
-- Symbol ::= Variable | Constant | OP
-- Variable ::= A | B | C | ...
-- Constant ::= a | b | c | ...
-- OP ::= + | -

-- displaces = makeDisplacementRules 
--     [ 'a' `displace` "a+b++b-a--aa-b+"
--     , 'b' `displace` "-a+bb++b+a--a-b"
--     ]
-- 最終的にこのように使いたい

-- パースされた結果は[Symbol]として返し、[moveFoward n] 関数のリストになる

-- moveFowardは角度(+/-の数と設定された角度)、前回の結果から新しい結果を生成する
-- より一般化した形にも対応したい
-- makeOriginalCorresponds -- Symbolと関数の対応の設定
--     [ ワンステップ進む [Symbol A, Symbol C]
--     , 二倍の長さ進む [Symbol B]
--     ]

