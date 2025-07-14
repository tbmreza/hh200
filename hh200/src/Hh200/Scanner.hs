{-# LANGUAGE NamedFieldPuns #-}

-- Re-export lexer and parser generated code.
module Hh200.Scanner
    ( module Hh200.Scanner
    , module L
    , module P
    ) where

import Hh200.Types
import L
import P

read :: String -> IO (Maybe CallItem)
read _x = do
    putStrLn "yea"
    return (Just ast1)

-- readFile :: FilePath -> Maybe CallItem
-- readFile _x = Just ast1
readFile :: FilePath -> CallItem
readFile _x = ast1

hhsStack :: CallItem -> HttpM ()
hhsStack CallItem { ci_request_spec = RequestSpec { url } } = do
    httpGet_ url

-- PICKUP empty HttpM ()? then more abstract syntax
fromHhs :: FilePath -> (ScriptConfig, HttpM ())
fromHhs x = (ScriptConfig { retries = 0, max_duration = Nothing }, hhsStack program) where
    program = Hh200.Scanner.readFile x
-- fromHhs x = (defaultScriptConfig, )

---------------------------
-- Test abstract syntax. --
---------------------------

ast1 :: CallItem
ast1 = CallItem {
    ci_deps = []
  , ci_name = "hello"
  , ci_request_spec = defaultRequestSpec
  , ci_response_spec = Just ResponseSpec { codes = [200], output = [] }
  }

-- "download image.jpg"
-- GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI
-- HTTP [200 201] ("/home/tbmreza/gh/hh200/hh200/img-{{row.username}}.jpg" fresh)
ast2images :: CallItem
ast2images = CallItem {
    ci_deps = []
  , ci_name = "download image.jpg"
  -- , ci_request_spec = RequestSpec { url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI" }
  , ci_request_spec = defaultRequestSpec
  , ci_response_spec = Just ResponseSpec { codes = [200], output = [] }
  }
