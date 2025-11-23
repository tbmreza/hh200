{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

--------------------------------------------------------------------------------
-- MAIN THESIS OF HH200 (Handzalah, 2024)
--------------------------------------------------------------------------------
--
-- 3-step:        Scanner.analyze  Execution.testOutsideWorld  Types.present
-- (deliverable)  (linter hints)   (reality)                   (counter-example)
--
--
-- linter hints:
--   lexer/parser, http idioms (GET with payload, webdav status code misuse)
--
-- reality:
--   status codes mismatch, duration, filesystem, thread cancelled, offline
--
-- counter-example:
--   hhs, curl

import Hh200.Cli

main :: IO ()
main = cli
