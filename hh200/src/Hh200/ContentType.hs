{-# LANGUAGE OverloadedStrings #-}

module Hh200.ContentType
    ( headerJson
    , extensionFor
    , saveBody
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Network.HTTP.Types.Header (HeaderName)
import           System.FilePath ((<.>), (</>))

headerJson :: (HeaderName, BS.ByteString)
headerJson = ("Content-Type", "application/json")

-- | Map MIME types to file extensions
extensionFor :: BS.ByteString -> String
extensionFor raw = match (BSC.takeWhile (/= ';') raw) where
    match "application/pdf" = "pdf"
    match "audio/mpeg" = "mp3"
    match "image/gif" = "gif"
    match "multipart/form-data" = "txt" -- Often processed, but saving as txt/log for debug
    match "text/csv" = "csv"
    match "video/mp4" = "mp4"
    match "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "xlsx"
    match "application/json" = "json"
    match "text/html" = "html"
    match "text/plain" = "txt"
    match _ = "dat" -- Fallback

-- | Save the body to a file with the appropriate extension based on Content-Type
saveBody :: FilePath -> String -> BS.ByteString -> BS.ByteString -> IO FilePath
saveBody dir baseName contentType body = do
    let ext = extensionFor contentType
        filename = dir </> (baseName <.> ext)
    BS.writeFile filename body
    pure filename
