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

import qualified Data.ByteString.Lazy.Char8 as L8

read :: String -> IO (Maybe CallItem)
read _x = do
    putStrLn "yea"
    return (Just ci)

-- stackHh :: [CallItem] -> HttpM ()
-- stackHh [CallItem { ci_deps, ci_name, ci_request_spec = RequestSpec { method, url } }] = do
--     httpGet_ url
--     -- httpGet url
--     -- httpGet_ url

    -- #! [user1 user2]
    --
    -- "download image.jpg"
    -- GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI
    -- HTTP [200 201] ("/home/tbmreza/gh/hh200/img-{{hh.str}}.jpg" fresh)

rs = RequestSpec
      { method = "GET"
      -- , url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI"
      , url = "http://localhost:9999/l"
      , headers = []
      , payload = ""
      , opts = []
      }
rp = ResponseSpec
      { codes = [200, 201]
      , output = ["/home/tbmreza/gh/hh200/img-.jpg"]
      }
ci = CallItem
      { ci_deps = []
      , ci_name = "download image.jpg"
      , ci_request_spec = rs
      , ci_response_spec = Just rp
      }

-- Abstract syntax for downloading 2 parallel files.
fromHhs :: FilePath -> IO (ScriptConfig, HttpM L8.ByteString)
fromHhs x = do
    let Script { config = c, call_items = cis } = Script { config = ScriptConfig { retries = 0, max_duration = Nothing, subjects = ["user1", "user2", "user1", "user2"] }, call_items = [ci]}
    -- let Script { config = c, call_items = cis } = Script { config = ScriptConfig { retries = 0, max_duration = Nothing, subjects = [] }, call_items = [ci, ci]}
    return (c, stackHh cis)
-- fromHhs :: FilePath -> IO (ScriptConfig, HttpM ())
-- fromHhs x = do
--     let Script { config = c, call_items = cis } = Script { config = ScriptConfig { retries = 0, max_duration = Nothing, subjects = [] }, call_items = [ci]}
--     return (c, stackHh cis)

---------------------------
-- Test abstract syntax. --
---------------------------

-- ast1 :: CallItem
-- ast1 = CallItem {
--     ci_deps = []
--   , ci_name = "hello"
--   , ci_request_spec = defaultRequestSpec
--   , ci_response_spec = Just ResponseSpec { codes = [200], output = [] }
--   }

-- -- "download image.jpg"
-- -- GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI
-- -- HTTP [200 201] ("/home/tbmreza/gh/hh200/hh200/img-{{row.username}}.jpg" fresh)
-- ast2images :: CallItem
-- ast2images = CallItem {
--     ci_deps = []
--   , ci_name = "download image.jpg"
--   -- , ci_request_spec = RequestSpec { url = "https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI" }
--   , ci_request_spec = defaultRequestSpec
--   , ci_response_spec = Just ResponseSpec { codes = [200], output = [] }
--   }
