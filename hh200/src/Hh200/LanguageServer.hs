module Hh200.LanguageServer (runTcp) where

import Language.LSP.VFS (virtualFileText, VirtualFile)

runTcp :: Int -> IO ()
runTcp port = do
    putStrLn $ "Run hh200 language server on port " ++ show port
    putStrLn "Not implemented yet"
