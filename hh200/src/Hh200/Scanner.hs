{-# LANGUAGE NamedFieldPuns #-}

-- Re-export lexer and parser generated code.
module Hh200.Scanner
    ( module Hh200.Scanner
    , module L
    , module P
    ) where

import System.FilePath
import Hh200.Types
import L
import P

---------------------------
-- Test abstract syntax. --
---------------------------

ast1 :: Mini
ast1 = Mini {
    mdeps = []
  , mname = "hello"
  , mrequest_spec = RequestSpec { url = "http://localhost:9999/yea" }
  , mresponse_spec = ResponseSpec { codes = [200] }
  }

-- "download image.jpg"
-- GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI
-- HTTP [200 201] ("/home/tbmreza/gh/hh200/hh200/img-{{row.username}}.jpg" fresh)
ast2images :: Mini
ast2images = Mini {
    mdeps = []
  , mname = "download image.jpg"
  -- , mrequest_spec = RequestSpec { url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI" }
  , mrequest_spec = RequestSpec { url = "http://localhost:9999/o" }
  , mresponse_spec = ResponseSpec { codes = [200] }
  }

read :: FilePath -> Mini
read x = ast1

hhsStack :: Mini -> HttpM ()
hhsStack Mini { mrequest_spec = RequestSpec { url } } = do
    httpGet_ url

fromHhs :: FilePath -> HttpM ()
fromHhs x = hhsStack $ Hh200.Scanner.read x
