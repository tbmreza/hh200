{-# LANGUAGE OverloadedStrings #-}

-- | Data model for the subset of curl's test file format that hh200 consumes.
--
-- curl's tests are pseudo-XML files (see upstream docs/tests/FILEFORMAT.md).
-- This module holds a reduced representation: enough to (a) replay the
-- server's <reply> bytes, (b) derive the hh200 request from <command>, and
-- (c) check the request against <verify><protocol>.
module CurlTest.Types
    ( CurlTest (..)
    , Section (..)
    , Reply (..)
    , Verify (..)
    , RequestPlan (..)
    , Expected (..)
    , Context (..)
    , emptyReply
    , emptyVerify
    ) where

import qualified Data.ByteString.Char8 as C8

-- | A single curl test case, reduced to the supported subset.
data CurlTest = CurlTest
    { ctNumber   :: Int
    , ctKeywords :: [String]
    , ctName     :: String
    , ctServer   :: String
    , ctFeatures :: [String]
    , ctCommand  :: String
    , ctStdin    :: Maybe String
    , ctReply    :: Reply
    , ctVerify   :: Verify
    } deriving (Show, Eq)

-- | A named pseudo-XML section with attributes and a raw body.
data Section = Section
    { secName  :: String              -- ^ e.g. @data@, @data1000@, @protocol@
    , secAttrs :: [(String, String)]  -- ^ @attribute=value@ pairs
    , secBody  :: String              -- ^ raw body, newlines intact
    } deriving (Show, Eq)

-- | The server-side data to send back.
data Reply = Reply
    { replyData  :: Maybe Section  -- ^ @<data>@, the primary reply
    , replyParts :: [Section]      -- ^ @<dataN>@ numbered parts (unsupported)
    } deriving (Show, Eq)

data Verify = Verify
    { vProtocol  :: Maybe Section  -- ^ @<protocol>@ expectation
    , vErrorcode :: [Int]          -- ^ accepted curl exit codes
    , vStdout    :: Maybe String
    , vStderr    :: Maybe String
    , vStrip     :: [String]       -- ^ regexes to strip before comparing
    } deriving (Show, Eq)

emptyReply :: Reply
emptyReply = Reply Nothing []

emptyVerify :: Verify
emptyVerify = Verify Nothing [] Nothing Nothing []

-- | What hh200 is expected to do with the replayed response.
data Expected
    = ExpectOk           -- ^ assertion passes (status matches)
    | ExpectHttpError    -- ^ transfer fails (empty reply, connection error, ...)
    | ExpectAssertFail   -- ^ status assertion fails (non-2xx reply)
    deriving (Show, Eq)

-- | The hh200 request derived from @<command>@ (host/port resolved later).
data RequestPlan = RequestPlan
    { rpMethod :: String      -- ^ e.g. @GET@, @PUT@
    , rpTarget :: String      -- ^ request-target, e.g. @/1@ or @/?mooo/59@
    , rpBodyBytes :: C8.ByteString  -- ^ request body bytes
    } deriving (Show, Eq)

-- | Macro-substitution environment used when rendering @<reply>@/@<protocol>@.
data Context = Context
    { ctxNumber :: Int
    , ctxHost   :: String
    , ctxPort   :: Int
    } deriving (Show, Eq)
