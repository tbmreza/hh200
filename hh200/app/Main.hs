module Main (main) where

--------------------------------------------------------------------------------
-- MAIN THESIS OF HH200 (Handzalah, 2024)
--------------------------------------------------------------------------------
--
--                                 Execution.testRps           Graph.connect
--                                 Execution.testShotgun       Graph.plot
-- 3-step:        Scanner.analyze  Execution.testOutsideWorld  Cli.present
-- (deliverable)  (linter hints)   (reality)                   (presentation)
--
--
-- linter hints:
--   lexer/parser, http idioms (GET with payload, webdav status code misuse)
--
-- reality:
--   status codes mismatch, duration, filesystem, thread cancelled, offline
--
-- presentation:
--   hhs curl counter-example, gnuplot
--

import Hh200.Cli

main :: IO ()
main = cli
