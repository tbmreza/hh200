{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.LSP.Server
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import Control.Monad.IO.Class
import qualified Data.Text.IO as T

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
      liftIO $ T.putStrLn "Server initialized"
  ]

main :: IO ()
main = do
  _ <- runServer $ ServerDefinition
    { defaultConfig = ()
    , configSection = "hh200d"
    , parseConfig = const $ const $ Right ()
    , onConfigChange = const $ pure ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = \_caps -> handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions
    }
  pure ()
