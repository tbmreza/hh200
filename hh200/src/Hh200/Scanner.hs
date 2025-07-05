{-# LANGUAGE NamedFieldPuns #-}

module Hh200.Scanner where

import System.FilePath
import Hh200.Types
import P

---------------------------
-- Test abstract syntax. --
---------------------------

ast1 :: Mini
ast1 = Mini { m_url = "http://localhost:9999/yea" }

read :: FilePath -> Mini
read x = ast1

hhsStack :: Mini -> HttpM ()
hhsStack Mini { m_url } = do
    httpGet_ m_url

fromHhs :: FilePath -> HttpM ()
fromHhs x = hhsStack $ Hh200.Scanner.read x
