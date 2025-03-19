module Lsystem 
    (

    ) where

-- A -> A - B という式は
-- 'A' `displace` "A-B"
-- と表す

-- dispace :: 一文字の変数 -> 適切な記号が使われた式 -> DisplacRule
-- char -> 一文字の変数 String -> 適切な記号が使われた式
-- 上の二つがあればok

import Lsystem.Parser
import Lsystem.Core