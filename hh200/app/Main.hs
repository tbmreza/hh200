{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Hh200.Cli

-- -- Delete language extension and this block before v1  {
-- import qualified Network.HTTP.Client as Prim
-- import qualified Data.Aeson as Json
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.HashMap.Strict as HM
--
-- _keepCompiling :: IO ()
-- _keepCompiling = do
--     let d :: HM.HashMap String String = HM.fromList [("username", "admin0"), ("password", "admin1234")]
--     let ed :: L8.ByteString = Json.encode d
--     let body = Prim.RequestBodyLBS $ Json.encode d
--     return ()
-- -- }

main :: IO ()
main = cli
