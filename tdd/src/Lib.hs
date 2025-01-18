module Lib
    ( cli
    ) where

import Options.Applicative
-- import Hh200.Types


data Args = Args
    { source  :: Maybe String
    , version :: Bool
    }


args :: Parser Args
args = Args
    <$> optional (argument str (metavar "SOURCE"
                             <> help "Path of source program"))
    <*> switch ( long "version"
              <> short 'V'
              <> help "Print version info and exit" )


go :: Args -> IO ()

-- hh200 --version
go (Args _ True) = putStrLn "1.0"

-- hh200 input.hhs
go (Args (Just s) _) = do
    -- ??:
    -- learn State monads
    -- PICKUP Vm module; tuple
    -- httpClientCall "https://httpbin.org/anything"
    return ()


cli :: IO ()
cli = go =<< execParser opts where
    opts = info (args <**> helper) (fullDesc
                                 <> header "Run hh200 scripts")
