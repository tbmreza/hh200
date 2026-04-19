{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# LANGUAGE ScopedTypeVariables #-}
module P where

import Debug.Trace

import           Data.List (intercalate, isPrefixOf, inits, isSuffixOf)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Control.Exception (try, SomeException)
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Types.Status (mkStatus)
import qualified Network.HTTP.Client as HC

import qualified BEL
import           Hh200.Types
import           L
import qualified Data.Function as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Function as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Control.Monad as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.1.4

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Happy_Prelude.Int
        | HappyAbsSyn5 (E Script)
        | HappyAbsSyn6 (E [CallItem])
        | HappyAbsSyn7 (E CallItem)
        | HappyAbsSyn8 (DepsClause)
        | HappyAbsSyn9 ([String])
        | HappyAbsSyn10 (E RequestSpec)
        | HappyAbsSyn11 ((Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare, Maybe RequestSquare))
        | HappyAbsSyn12 (RequestSquare)
        | HappyAbsSyn13 (LexedUrl)
        | HappyAbsSyn14 (ResponseSpec)
        | HappyAbsSyn15 ((Maybe ResponseSquare, Maybe ResponseSquare))
        | HappyAbsSyn16 (ResponseSquare)
        | HappyAbsSyn19 ([Int])
        | HappyAbsSyn20 (RhsDict)
        | HappyAbsSyn21 (Binding)
        | HappyAbsSyn22 (())

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["d","identifier","newline","\"=\"","\"/\"","\".\"","\",\"","\":\"","\"\\\"\"","\"{\"","\"}\"","\"(\"","\")\"","\"[\"","\"]\"","\"then\"","\"HTTP\"","\"Configs\"","\"Query\"","\"Form\"","\"MultipartFormData\"","\"Cookies\"","\"Captures\"","\"Asserts\"","method","url_proto","s","braced","rhs","line","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\xe6\xff\xff\xff\x7f\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xff\xff\x1d\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x74\x00\x00\x00\x4b\x00\x00\x00\x52\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\x8d\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x00\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x73\x00\x00\x00\x9b\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x1a\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x9d\x00\x00\x00\x9e\x00\x00\x00\x9f\x00\x00\x00\xa0\x00\x00\x00\xa1\x00\x00\x00\xa3\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x99\x00\x00\x00\x01\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x22\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x99\x00\x00\x00\x99\x00\x00\x00\x99\x00\x00\x00\x99\x00\x00\x00\x99\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\xb1\x00\x00\x00\xb2\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x00\x00\xb4\x00\x00\x00\xb4\x00\x00\x00\xb4\x00\x00\x00\xb4\x00\x00\x00\xb5\x00\x00\x00\xb5\x00\x00\x00\xb5\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x03\x00\x00\x00\xa7\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\xa9\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x00\x00\xaa\x00\x00\x00\x00\x00\x00\x00\xb6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x62\x00\x00\x00\xac\x00\x00\x00\x00\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\x00\x00\xb3\x00\x00\x00\xb7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\x00\x00\x00\xbb\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x00\x00\x00\x00\x00\x00\x00\xbd\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x00\x00\x44\x00\x00\x00\xbf\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x43\x00\x00\x00\xcb\x00\x00\x00\x3c\x00\x00\x00\xc2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x00\x00\x00\x00\x00\x00\x00\xc3\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\x00\x00\x00\x00\xc5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\x00\x00\x00\xc8\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x7c\x00\x00\x00\xc9\x00\x00\x00\x8f\x00\x00\x00\xce\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\x00\x00\x00\xcd\x00\x00\x00\xcf\x00\x00\x00\xd0\x00\x00\x00\xd1\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x00\x00\xd3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\x00\x00\x00\x91\x00\x00\x00\x93\x00\x00\x00\x95\x00\x00\x00\x97\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x00\x00\xd5\x00\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\xd6\x00\x00\x00\x00\x00\x00\x00\xd7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x00\x00\x00\x00\x00\x00\x00\xd9\x00\x00\x00\xdb\x00\x00\x00\xdb\x00\x00\x00\xdb\x00\x00\x00\xdb\x00\x00\x00\xdb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xbe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xff\xff\xbe\xff\xff\xff\xbd\xff\xff\xff\x00\x00\x00\x00\xde\xff\xff\xff\xf5\xff\xff\xff\xbe\xff\xff\xff\xe6\xff\xff\xff\xf9\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xca\xff\xff\xff\xbe\xff\xff\xff\xc3\xff\xff\xff\xce\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xff\xff\x00\x00\x00\x00\xf4\xff\xff\xff\xbe\xff\xff\xff\xfd\xff\xff\xff\xbe\xff\xff\xff\xfc\xff\xff\xff\xfb\xff\xff\xff\xf7\xff\xff\xff\xcf\xff\xff\xff\xbe\xff\xff\xff\xc5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xc2\xff\xff\xff\xd1\xff\xff\xff\xbe\xff\xff\xff\xcc\xff\xff\xff\xbe\xff\xff\xff\xd0\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\xbe\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\xbe\xff\xff\xff\xbe\xff\xff\xff\xe7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xff\xff\xe9\xff\xff\xff\xbe\xff\xff\xff\xe4\xff\xff\xff\xbe\xff\xff\xff\xe8\xff\xff\xff\xbe\xff\xff\xff\xd3\xff\xff\xff\xcd\xff\xff\xff\xd4\xff\xff\xff\xd5\xff\xff\xff\xbe\xff\xff\xff\xbf\xff\xff\xff\xc0\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xd2\xff\xff\xff\xc4\xff\xff\xff\xbe\xff\xff\xff\xd8\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\xc8\xff\xff\xff\xd9\xff\xff\xff\xeb\xff\xff\xff\xe5\xff\xff\xff\xec\xff\xff\xff\xed\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xef\xff\xff\xff\xbe\xff\xff\xff\xee\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xf0\xff\xff\xff\xf1\xff\xff\xff\xf2\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\xff\xff\xff\xbe\xff\xff\xff\xcb\xff\xff\xff\xd6\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\xd7\xff\xff\xff\xbe\xff\xff\xff\xda\xff\xff\xff\xdb\xff\xff\xff\xdc\xff\xff\xff\xbe\xff\xff\xff\xc7\xff\xff\xff\xbe\xff\xff\xff\xdf\xff\xff\xff\xe0\xff\xff\xff\xe1\xff\xff\xff\xe2\xff\xff\xff\xe3\xff\xff\xff\xf3\xff\xff\xff\xc6\xff\xff\xff\xdd\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x04\x00\x00\x00\x20\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x0f\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x12\x00\x00\x00\x03\x00\x00\x00\x1d\x00\x00\x00\x0f\x00\x00\x00\x11\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x20\x00\x00\x00\x0f\x00\x00\x00\x1f\x00\x00\x00\x1d\x00\x00\x00\x0f\x00\x00\x00\x1d\x00\x00\x00\x04\x00\x00\x00\x1d\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x1d\x00\x00\x00\x05\x00\x00\x00\x1d\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\x1d\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x04\x00\x00\x00\x1d\x00\x00\x00\x1d\x00\x00\x00\x1d\x00\x00\x00\x1d\x00\x00\x00\x1c\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x1d\x00\x00\x00\x1d\x00\x00\x00\x1d\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x1b\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x07\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x07\x00\x00\x00\x07\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x08\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x11\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x08\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x1c\x00\x00\x00\x10\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x11\x00\x00\x00\x1c\x00\x00\x00\x04\x00\x00\x00\x1e\x00\x00\x00\x11\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x04\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x1f\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x11\x00\x00\x00\x04\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x0e\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x11\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x18\x00\x00\x00\xbe\xff\xff\xff\x03\x00\x00\x00\x19\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x18\x00\x00\x00\x0b\x00\x00\x00\x18\x00\x00\x00\x0b\x00\x00\x00\x18\x00\x00\x00\x0b\x00\x00\x00\x19\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x1a\x00\x00\x00\x18\x00\x00\x00\x33\x00\x00\x00\x37\x00\x00\x00\x02\x00\x00\x00\x37\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\xbe\xff\xff\xff\xbe\xff\xff\xff\xbe\xff\xff\xff\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\xbe\xff\xff\xff\x19\x00\x00\x00\x73\x00\x00\x00\x38\x00\x00\x00\x37\x00\x00\x00\x3c\x00\x00\x00\x0b\x00\x00\x00\x57\x00\x00\x00\x18\x00\x00\x00\x37\x00\x00\x00\x37\x00\x00\x00\x37\x00\x00\x00\x19\x00\x00\x00\x2f\x00\x00\x00\x1d\x00\x00\x00\x4e\x00\x00\x00\x1c\x00\x00\x00\x09\x00\x00\x00\x45\x00\x00\x00\x37\x00\x00\x00\x19\x00\x00\x00\x19\x00\x00\x00\x0b\x00\x00\x00\x49\x00\x00\x00\x67\x00\x00\x00\x68\x00\x00\x00\x79\x00\x00\x00\x1d\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x42\x00\x00\x00\x34\x00\x00\x00\x6c\x00\x00\x00\x77\x00\x00\x00\x7d\x00\x00\x00\x64\x00\x00\x00\x34\x00\x00\x00\x0d\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2c\x00\x00\x00\x43\x00\x00\x00\x39\x00\x00\x00\x34\x00\x00\x00\x46\x00\x00\x00\x2c\x00\x00\x00\x65\x00\x00\x00\x35\x00\x00\x00\x15\x00\x00\x00\x46\x00\x00\x00\x46\x00\x00\x00\x3a\x00\x00\x00\x15\x00\x00\x00\x26\x00\x00\x00\x47\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x5d\x00\x00\x00\x6a\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x21\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x30\x00\x00\x00\x13\x00\x00\x00\x09\x00\x00\x00\x2b\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x31\x00\x00\x00\xf6\xff\xff\xff\xf6\xff\xff\xff\x2c\x00\x00\x00\x2d\x00\x00\x00\x1f\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x30\x00\x00\x00\x13\x00\x00\x00\x09\x00\x00\x00\x54\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x4c\x00\x00\x00\x52\x00\x00\x00\x55\x00\x00\x00\x15\x00\x00\x00\x75\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x30\x00\x00\x00\x13\x00\x00\x00\x23\x00\x00\x00\x2c\x00\x00\x00\x30\x00\x00\x00\x13\x00\x00\x00\x77\x00\x00\x00\x2a\x00\x00\x00\x0b\x00\x00\x00\x2b\x00\x00\x00\x7b\x00\x00\x00\x3e\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x18\x00\x00\x00\x0b\x00\x00\x00\x73\x00\x00\x00\x15\x00\x00\x00\x83\x00\x00\x00\x15\x00\x00\x00\x82\x00\x00\x00\x15\x00\x00\x00\x81\x00\x00\x00\x15\x00\x00\x00\x80\x00\x00\x00\x15\x00\x00\x00\x54\x00\x00\x00\x7f\x00\x00\x00\x15\x00\x00\x00\x51\x00\x00\x00\x0b\x00\x00\x00\x63\x00\x00\x00\x62\x00\x00\x00\x61\x00\x00\x00\x60\x00\x00\x00\x5f\x00\x00\x00\x37\x00\x00\x00\x0b\x00\x00\x00\x18\x00\x00\x00\x7f\x00\x00\x00\x0b\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x1e\x00\x00\x00\x0f\x00\x00\x00\x38\x00\x00\x00\x2f\x00\x00\x00\x0e\x00\x00\x00\x28\x00\x00\x00\x2c\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x20\x00\x00\x00\x52\x00\x00\x00\x4f\x00\x00\x00\x4e\x00\x00\x00\x4b\x00\x00\x00\x4a\x00\x00\x00\x49\x00\x00\x00\x45\x00\x00\x00\x3c\x00\x00\x00\x46\x00\x00\x00\x63\x00\x00\x00\x5c\x00\x00\x00\x5b\x00\x00\x00\x5a\x00\x00\x00\x59\x00\x00\x00\x58\x00\x00\x00\x57\x00\x00\x00\x74\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x6d\x00\x00\x00\x6c\x00\x00\x00\x69\x00\x00\x00\x68\x00\x00\x00\x84\x00\x00\x00\x7d\x00\x00\x00\x7a\x00\x00\x00\x79\x00\x00\x00\x86\x00\x00\x00\x85\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 66) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x07\x00\x00\x00\x09\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x09\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x02\x00\x00\x00\x09\x00\x00\x00\x02\x00\x00\x00\x09\x00\x00\x00\x02\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x03\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x05\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x0c\x00\x00\x00\x04\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x02\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 33 :: Happy_Prelude.Int
happy_n_nonterms = 18 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

happyReduce_1 = happySpecReduce_2  0# happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_2)
        _
         =  HappyAbsSyn5
                 (happy_var_2 >>= \is -> returnE Script { kind = Regular, config = defaultScriptConfig, callItems = is }
        )
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  1# happyReduction_2
happyReduction_2 _
        (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1 >>= \i -> returnE [i]
        )
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  1# happyReduction_3
happyReduction_3 _
        (HappyAbsSyn7  happy_var_2)
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1 >>= \is -> happy_var_2 >>= \i -> returnE (is ++ [i])
        )
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  2# happyReduction_4
happyReduction_4 (HappyAbsSyn14  happy_var_3)
        (HappyAbsSyn10  happy_var_2)
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_2 >>= \r -> returnE CallItem { ciDeps = deps happy_var_1, ciName = itemName happy_var_1, ciRequestSpec = r, ciResponseSpec = Just happy_var_3 }
        )
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  2# happyReduction_5
happyReduction_5 (HappyAbsSyn10  happy_var_2)
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_2 >>= \r -> returnE CallItem { ciDeps = deps happy_var_1, ciName = itemName happy_var_1, ciRequestSpec = r, ciResponseSpec = Nothing }
        )
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  2# happyReduction_6
happyReduction_6 (HappyAbsSyn14  happy_var_2)
        (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_1 >>= \r -> returnE CallItem { ciDeps = [], ciName = "", ciRequestSpec = r, ciResponseSpec = Just happy_var_2 }
        )
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  2# happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn7
                 (happy_var_1 >>= \r -> returnE CallItem { ciDeps = [], ciName = "", ciRequestSpec = r, ciResponseSpec = Nothing }
        )
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  3# happyReduction_8
happyReduction_8 (HappyTerminal (QUOTED _ happy_var_3))
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn8
                 (DepsClause { deps = happy_var_1, itemName = happy_var_3 }
        )
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  3# happyReduction_9
happyReduction_9 (HappyTerminal (QUOTED _ happy_var_1))
         =  HappyAbsSyn8
                 (DepsClause { deps = [], itemName = happy_var_1 }
        )
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  4# happyReduction_10
happyReduction_10 (HappyTerminal (QUOTED _ happy_var_1))
         =  HappyAbsSyn9
                 ([happy_var_1]
        )
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  4# happyReduction_11
happyReduction_11 (HappyTerminal (QUOTED _ happy_var_2))
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1 ++ [happy_var_2]
        )
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 7# 5# happyReduction_12
happyReduction_12 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_6)) `HappyStk`
        (HappyAbsSyn11  happy_var_5) `HappyStk`
        (HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = happy_var_1,    rqUrl = happy_var_2, rqHeaders = happy_var_4,               rqSquares = happy_var_5,               rqBody = happy_var_6 }
                                                               trace "reqA" $ pure r
        ) `HappyStk` happyRest

happyReduce_13 = happyReduce 6# 5# happyReduction_13
happyReduction_13 (_ `HappyStk`
        (HappyAbsSyn11  happy_var_5) `HappyStk`
        (HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = happy_var_1,    rqUrl = happy_var_2, rqHeaders = happy_var_4,               rqSquares = happy_var_5,               rqBody = "" }
                                                               trace "reqB" $ pure r
        ) `HappyStk` happyRest

happyReduce_14 = happyReduce 6# 5# happyReduction_14
happyReduction_14 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_5)) `HappyStk`
        (HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = happy_var_1,    rqUrl = happy_var_2, rqHeaders = happy_var_4,               rqSquares = rqSquaresNothing, rqBody = happy_var_5 }
                                                               trace "reqC" $ pure r
        ) `HappyStk` happyRest

happyReduce_15 = happyReduce 6# 5# happyReduction_15
happyReduction_15 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_5)) `HappyStk`
        (HappyAbsSyn11  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = happy_var_1,    rqUrl = happy_var_2, rqHeaders = RhsDict HM.empty, rqSquares = happy_var_4,               rqBody = happy_var_5 }
                                                               trace "reqD" $ pure r
        ) `HappyStk` happyRest

happyReduce_16 = happyReduce 5# 5# happyReduction_16
happyReduction_16 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_4)) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = happy_var_1,    rqUrl = happy_var_2, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = happy_var_4 }
                                                               trace "reqE" $ pure r
        ) `HappyStk` happyRest

happyReduce_17 = happyReduce 5# 5# happyReduction_17
happyReduction_17 (_ `HappyStk`
        (HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_2) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = happy_var_1,    rqUrl = happy_var_2, rqHeaders = happy_var_4,               rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqF" $ pure r
        ) `HappyStk` happyRest

happyReduce_18 = happyReduce 5# 5# happyReduction_18
happyReduction_18 (_ `HappyStk`
        (HappyAbsSyn11  happy_var_4) `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = happy_var_1, rqHeaders = happy_var_3,               rqSquares = happy_var_4,               rqBody = "" }
                                                               trace "reqG" $ pure r
        ) `HappyStk` happyRest

happyReduce_19 = happyReduce 5# 5# happyReduction_19
happyReduction_19 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_4)) `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = happy_var_1, rqHeaders = happy_var_3,               rqSquares = rqSquaresNothing, rqBody = happy_var_4 }
                                                               trace "reqH" $ pure r
        ) `HappyStk` happyRest

happyReduce_20 = happyReduce 5# 5# happyReduction_20
happyReduction_20 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_4)) `HappyStk`
        (HappyAbsSyn11  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = happy_var_1, rqHeaders = RhsDict HM.empty, rqSquares = happy_var_3,               rqBody = happy_var_4 }
                                                               trace "reqI" $ pure r
        ) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  5# happyReduction_21
happyReduction_21 _
        (HappyAbsSyn13  happy_var_2)
        (HappyTerminal (METHOD _ happy_var_1))
         =  HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = happy_var_1,    rqUrl = happy_var_2, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqJ" $ pure r
        )
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4# 5# happyReduction_22
happyReduction_22 (_ `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = happy_var_1, rqHeaders = happy_var_3,               rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqK" $ pure r
        ) `HappyStk` happyRest

happyReduce_23 = happyReduce 4# 5# happyReduction_23
happyReduction_23 (_ `HappyStk`
        (HappyAbsSyn11  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = happy_var_1, rqHeaders = RhsDict HM.empty, rqSquares = happy_var_3,               rqBody = "" }
                                                               trace "reqL" $ pure r
        ) `HappyStk` happyRest

happyReduce_24 = happyReduce 4# 5# happyReduction_24
happyReduction_24 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_3)) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = happy_var_1, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = happy_var_3 }
                                                               trace "reqM" $ pure r
        ) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_2  5# happyReduction_25
happyReduction_25 _
        (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn10
                 (do
                                                               let r = RequestSpec { rqMethod = "GET", rqUrl = happy_var_1, rqHeaders = RhsDict HM.empty, rqSquares = rqSquaresNothing, rqBody = "" }
                                                               trace "reqN" $ pure r
        )
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  6# happyReduction_26
happyReduction_26 _
        (HappyAbsSyn12  happy_var_2)
        (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn11
                 (setRequestSquare happy_var_2 happy_var_1
        )
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  6# happyReduction_27
happyReduction_27 _
        (HappyAbsSyn12  happy_var_1)
         =  HappyAbsSyn11
                 (initRequestSquare happy_var_1
        )
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 5# 7# happyReduction_28
happyReduction_28 ((HappyAbsSyn20  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSquareConfigs happy_var_5
        ) `HappyStk` happyRest

happyReduce_29 = happyReduce 5# 7# happyReduction_29
happyReduction_29 ((HappyAbsSyn20  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSquareQuery happy_var_5
        ) `HappyStk` happyRest

happyReduce_30 = happyReduce 5# 7# happyReduction_30
happyReduction_30 ((HappyAbsSyn20  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSquareForm happy_var_5
        ) `HappyStk` happyRest

happyReduce_31 = happyReduce 5# 7# happyReduction_31
happyReduction_31 ((HappyAbsSyn20  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSquareMultipart happy_var_5
        ) `HappyStk` happyRest

happyReduce_32 = happyReduce 5# 7# happyReduction_32
happyReduction_32 ((HappyAbsSyn20  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSquareCookies happy_var_5
        ) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_1  8# happyReduction_33
happyReduction_33 (HappyTerminal (URL _ happy_var_1))
         =  HappyAbsSyn13
                 (if hasBalancedMustache happy_var_1 then
                      LexedUrlSegments (BEL.partitions (Text.pack happy_var_1))
                  else
                      LexedUrlFull happy_var_1
        )
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happyReduce 7# 9# happyReduction_34
happyReduction_34 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_6)) `HappyStk`
        (HappyAbsSyn15  happy_var_5) `HappyStk`
        (HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpA" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = happy_var_4,               rpSquares = happy_var_5,               rpBody = happy_var_6 }
        ) `HappyStk` happyRest

happyReduce_35 = happyReduce 6# 9# happyReduction_35
happyReduction_35 (_ `HappyStk`
        (HappyAbsSyn15  happy_var_5) `HappyStk`
        (HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpB" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = happy_var_4,               rpSquares = happy_var_5,               rpBody = "" }
        ) `HappyStk` happyRest

happyReduce_36 = happyReduce 6# 9# happyReduction_36
happyReduction_36 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_5)) `HappyStk`
        (HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpC" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = happy_var_4,               rpSquares = rpSquaresNothing, rpBody = happy_var_5 }
        ) `HappyStk` happyRest

happyReduce_37 = happyReduce 6# 9# happyReduction_37
happyReduction_37 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_5)) `HappyStk`
        (HappyAbsSyn15  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpD" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = RhsDict HM.empty, rpSquares = happy_var_4,               rpBody = happy_var_5 }
        ) `HappyStk` happyRest

happyReduce_38 = happyReduce 4# 9# happyReduction_38
happyReduction_38 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_3)) `HappyStk`
        (HappyAbsSyn15  happy_var_2) `HappyStk`
        (HappyAbsSyn20  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpE" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = happy_var_1,               rpSquares = happy_var_2,               rpBody = happy_var_3 }
        ) `HappyStk` happyRest

happyReduce_39 = happyReduce 4# 9# happyReduction_39
happyReduction_39 ((HappyAbsSyn20  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpF" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = happy_var_4,               rpSquares = rpSquaresNothing, rpBody = "" }
        ) `HappyStk` happyRest

happyReduce_40 = happyReduce 5# 9# happyReduction_40
happyReduction_40 (_ `HappyStk`
        (HappyAbsSyn15  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpG" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = RhsDict HM.empty, rpSquares = happy_var_4,               rpBody = "" }
        ) `HappyStk` happyRest

happyReduce_41 = happyReduce 5# 9# happyReduction_41
happyReduction_41 (_ `HappyStk`
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_4)) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (trace "rpH" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = happy_var_4 }
        ) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_3  9# happyReduction_42
happyReduction_42 _
        (HappyAbsSyn15  happy_var_2)
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn14
                 (trace "rpI" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = happy_var_1,               rpSquares = happy_var_2,               rpBody = "" }
        )
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  9# happyReduction_43
happyReduction_43 _
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_2))
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn14
                 (trace "rpJ" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = happy_var_1,               rpSquares = rpSquaresNothing, rpBody = happy_var_2 }
        )
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  9# happyReduction_44
happyReduction_44 _
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_2))
        (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn14
                 (trace "rpK" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = happy_var_1,               rpBody = happy_var_2 }
        )
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  9# happyReduction_45
happyReduction_45 _
        (HappyAbsSyn19  happy_var_2)
        _
         =  HappyAbsSyn14
                 (trace "rpL" $ ResponseSpec { rpStatuses = map statusFrom happy_var_2, rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = "" }
        )
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  9# happyReduction_46
happyReduction_46 _
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn14
                 (trace "rpM" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = happy_var_1, rpSquares = rpSquaresNothing,               rpBody = "" }
        )
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  9# happyReduction_47
happyReduction_47 _
        (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn14
                 (trace "rpN" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = happy_var_1,               rpBody = "" }
        )
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  9# happyReduction_48
happyReduction_48 _
        (HappyTerminal (BRACE_ENCLOSED _ happy_var_1))
         =  HappyAbsSyn14
                 (trace "rpO" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = happy_var_1 }
        )
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  9# happyReduction_49
happyReduction_49 _
         =  HappyAbsSyn14
                 (trace "rpP" $ ResponseSpec { rpStatuses = [],                rpResponseHeaders = RhsDict HM.empty, rpSquares = rpSquaresNothing, rpBody = "" }
        )

happyReduce_50 = happySpecReduce_3  10# happyReduction_50
happyReduction_50 _
        (HappyAbsSyn16  happy_var_2)
        (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn15
                 (setResponseSquare happy_var_2 happy_var_1
        )
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  10# happyReduction_51
happyReduction_51 _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn15
                 (initResponseSquare happy_var_1
        )
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 5# 11# happyReduction_52
happyReduction_52 ((HappyAbsSyn20  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (ResponseSquareCaptures happy_var_5
        ) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_1  11# happyReduction_53
happyReduction_53 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn16
                 (ResponseSquareAsserts happy_var_1
        )
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happyReduce 5# 12# happyReduction_54
happyReduction_54 ((HappyAbsSyn9  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (happy_var_5
        ) `HappyStk` happyRest

happyReduce_55 = happyReduce 4# 12# happyReduction_55
happyReduction_55 (_ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 ([]
        ) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_2  13# happyReduction_56
happyReduction_56 _
        (HappyTerminal (LINE _ happy_var_1))
         =  HappyAbsSyn9
                 ([happy_var_1]
        )
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  13# happyReduction_57
happyReduction_57 _
        (HappyTerminal (LINE _ happy_var_2))
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 ((happy_var_1 ++ [happy_var_2])
        )
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  14# happyReduction_58
happyReduction_58 (HappyTerminal (DIGITS _ happy_var_1))
         =  HappyAbsSyn19
                 ([read happy_var_1]
        )
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  14# happyReduction_59
happyReduction_59 (HappyTerminal (DIGITS _ happy_var_2))
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn19
                 (happy_var_1 ++ [read happy_var_2]
        )
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  15# happyReduction_60
happyReduction_60 (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn20
                 (RhsDict (HM.fromList [happy_var_1])
        )
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  15# happyReduction_61
happyReduction_61 (HappyAbsSyn21  happy_var_2)
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn20
                 (let (RhsDict acc) = happy_var_1 in
                              RhsDict (HM.insertWith (++) (fst happy_var_2) (snd happy_var_2) acc)
        )
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  16# happyReduction_62
happyReduction_62 _
        (HappyTerminal (IDENTIFIER _ happy_var_1))
         =  HappyAbsSyn21
                 ((Text.pack happy_var_1, [])
        )
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  16# happyReduction_63
happyReduction_63 _
        (HappyTerminal (QUOTED _ happy_var_2))
        (HappyTerminal (IDENTIFIER _ happy_var_1))
         =  HappyAbsSyn21
                 ((Text.pack happy_var_1, [BEL.R (Text.pack happy_var_2)])
        )
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  16# happyReduction_64
happyReduction_64 _
        (HappyTerminal (RHS _ happy_var_2))
        (HappyTerminal (IDENTIFIER _ happy_var_1))
         =  HappyAbsSyn21
                 ((Text.pack happy_var_1, [BEL.L (Text.pack (drop 1 happy_var_2))])
        )
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_0  17# happyReduction_65
happyReduction_65  =  HappyAbsSyn22
                 (
        )

happyReduce_66 = happySpecReduce_2  17# happyReduction_66
happyReduction_66 _
        _
         =  HappyAbsSyn22
                 (
        )

happyTerminalToTok term = case term of {
        DIGITS _ happy_dollar_dollar -> 2#;
        IDENTIFIER _ happy_dollar_dollar -> 3#;
        LN _ -> 4#;
        SEP _ -> 5#;
        SEP _ -> 6#;
        SEP _ -> 7#;
        SEP _ -> 8#;
        COLON _ -> 9#;
        QUOTE _ -> 10#;
        BRACE_OPN _ -> 11#;
        BRACE_CLS _ -> 12#;
        PAREN_OPN _ -> 13#;
        PAREN_CLS _ -> 14#;
        LIST_OPN _ -> 15#;
        LIST_CLS _ -> 16#;
        KW_THEN _ -> 17#;
        KW_HTTP _ -> 18#;
        KW_CONFIGS _ -> 19#;
        KW_QUERY _ -> 20#;
        KW_FORM _ -> 21#;
        KW_MULTIPART _ -> 22#;
        KW_COOKIES _ -> 23#;
        KW_CAPTURES _ -> 24#;
        KW_ASSERTS _ -> 25#;
        METHOD _ happy_dollar_dollar -> 26#;
        URL _ happy_dollar_dollar -> 27#;
        QUOTED _ happy_dollar_dollar -> 28#;
        BRACE_ENCLOSED _ happy_dollar_dollar -> 29#;
        RHS _ happy_dollar_dollar -> 30#;
        LINE _ happy_dollar_dollar -> 31#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 32# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 32# tk explist resume tks = happyReport' tks explist resume
happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (\tks -> resume (Happy_Prelude.tail tks))


happyThen :: () => (E a) -> (a -> (E b)) -> (E b)
happyThen = (thenE)
happyReturn :: () => a -> (E a)
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyFmap1 f m tks = happyThen (m tks) (\a -> happyReturn (f a))
happyReturn1 :: () => a -> b -> (E a)
happyReturn1 = \a tks -> (returnE) a
happyReport' :: () => [(Token)] -> [Happy_Prelude.String] -> ([(Token)] -> (E a)) -> (E a)
happyReport' = (\tokens expected resume -> (parseError) tokens)

happyAbort :: () => [(Token)] -> (E a)
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

parse tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


setResponseSquare sq (a, c) = case sq of
    ResponseSquareAsserts _ ->  (Just sq, c)
    ResponseSquareCaptures _ -> (a,       Just sq)
    _                      ->   (a,       c)

initResponseSquare sq = case sq of
    ResponseSquareCaptures _ -> (Just sq, Nothing)
    ResponseSquareAsserts _ ->  (Nothing, Just sq)

setRequestSquare sq (c, q, f, m, k) = case sq of
    RequestSquareConfigs _ ->   (Just sq, q,       f,       m,       k)
    RequestSquareQuery _ ->     (c,       Just sq, f,       m,       k)
    RequestSquareForm _ ->      (c,       q,       Just sq, m,       k)
    RequestSquareMultipart _ -> (c,       q,       f,       Just sq, k)
    RequestSquareCookies _ ->   (c,       q,       f,       m,       Just sq)
    _                      ->   (c,       q,       f,       m,       k)

initRequestSquare sq = case sq of
    RequestSquareConfigs _ ->   (Just sq, Nothing, Nothing, Nothing, Nothing)
    RequestSquareQuery _ ->     (Nothing, Just sq, Nothing, Nothing, Nothing)
    RequestSquareForm _ ->      (Nothing, Nothing, Just sq, Nothing, Nothing)
    RequestSquareMultipart _ -> (Nothing, Nothing, Nothing, Just sq, Nothing)
    RequestSquareCookies _ ->   (Nothing, Nothing, Nothing, Nothing, Just sq)

hasBalancedMustache :: String -> Bool
hasBalancedMustache s = countOpen > 0 && countOpen == countClose && countOpen == length (filter isOpenPair allPrefixes)
  where
    s' = filter (`elem` ['{', '}']) s
    countOpen = length $ filter isOpenPair allPrefixes
    countClose = length $ filter isClosePair allPrefixes
    allPrefixes = take (length s' - 1) (inits s')
    isOpenPair p = "{{" `isSuffixOf` p
    isClosePair p = "}}" `isSuffixOf` p
    inits [] = []
    inits xs = xs : inits (init xs)

-- statusFrom :: Int -> Status
statusFrom n = mkStatus n ""

stripColon :: String -> String
stripColon s = dropWhile (\c -> c == ':' || c == ' ') s

mergeRhsDicts :: RhsDict -> RhsDict -> RhsDict
mergeRhsDicts (RhsDict a) (RhsDict b) = RhsDict (HM.unionWith (++) a b)

parseError :: [Token] -> E a
parseError tokens = failE ("Parse error on tokens:\n" ++ prettyTokens tokens) tokens

prettyTokens :: [Token] -> String
prettyTokens = intercalate "\n" . map (("  " ++) . prettyToken) . take 10

prettyToken :: Token -> String
prettyToken t = case t of
    LN p           -> "LN " ++ showPos p
    DIGITS p s     -> "DIGITS " ++ showPos p ++ " " ++ show s
    IDENTIFIER p s -> "IDENTIFIER " ++ showPos p ++ " " ++ show s
    SEP p          -> "SEP " ++ showPos p
    METHOD p s     -> "METHOD " ++ showPos p ++ " " ++ show s
    VERSION p s    -> "VERSION " ++ showPos p ++ " " ++ show s
    BRACE_OPN p    -> "BRACE_OPN " ++ showPos p
    BRACE_CLS p    -> "BRACE_CLS " ++ showPos p
    PAREN_OPN p    -> "PAREN_OPN " ++ showPos p
    PAREN_CLS p    -> "PAREN_CLS " ++ showPos p
    LIST_OPN p     -> "LIST_OPN " ++ showPos p
    LIST_CLS p     -> "LIST_CLS " ++ showPos p
    COLON p        -> "COLON " ++ showPos p
    QUOTE p        -> "QUOTE " ++ showPos p
    KW_THEN p      -> "KW_THEN " ++ showPos p
    KW_HTTP p      -> "KW_HTTP " ++ showPos p
    KW_CONFIGS p   -> "KW_CONFIGS " ++ showPos p
    KW_CAPTURES p  -> "KW_CAPTURES " ++ showPos p
    KW_ASSERTS p   -> "KW_ASSERTS " ++ showPos p
    URL p s        -> "URL " ++ showPos p ++ " " ++ show s
    QUOTED p s     -> "QUOTED " ++ showPos p ++ " " ++ show s
    BRACED p s     -> "BRACED " ++ showPos p ++ " " ++ show s
    BRACE_ENCLOSED p s -> "BRACE_ENCLOSED " ++ showPos p ++ " " ++ show s
    RHS p s        -> "RHS " ++ showPos p ++ " " ++ show s
    JSONPATH p s   -> "JSONPATH " ++ showPos p ++ " " ++ show s
    LINE p s       -> "LINE " ++ showPos p ++ " " ++ show s
    EOF p          -> "EOF " ++ showPos p

showPos :: AlexPosn -> String
showPos (AlexPn _ l c) = "(" ++ show l ++ ":" ++ show c ++ ")"


type E a = ExceptT (String, [Token]) IO a

thenE :: E a -> (a -> E b) -> E b
thenE = (>>=)

returnE :: a -> E a
returnE = return

failE :: String -> [Token] -> E a
failE err tokens = throwE (err, tokens)

catchE :: E a -> (String -> [Token] -> E a) -> E a
catchE m k = Control.Monad.Trans.Except.catchE m (\(e, ts) -> k e ts)
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define INVALID_TOK -1#
#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) Happy_Prelude.$
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO Happy_Prelude.$ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    Happy_Prelude.return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++
              ",\ttoken: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# rule) Happy_Prelude.++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | Happy_Prelude.otherwise = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token ERROR_TOK. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no ERROR_TOK action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is ERROR_TOK, phase (1) has failed and we might try
-- phase (2).
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | Happy_Prelude.otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", ")
        if Happy_Prelude.null catch_frames_new
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states " Happy_Prelude.++ Happy_Prelude.show (Happy_Prelude.map (happy_list_to_list . Happy_Prelude.fst) (Happy_Prelude.reverse catch_frames_new)) Happy_Prelude.++ ".\n")
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", into state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
            Happy_Prelude.null (Happy_Prelude.filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_frames -- MK_ERROR_TOKEN(i) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ "\n")
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = DEBUG_TRACE("found expected token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ " after shifting from " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# catch_st) Happy_Prelude.++ ": " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | Happy_Prelude.otherwise
      = DEBUG_TRACE("discard token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@(((HappyCons st sts),_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          DEBUG_TRACE("trying token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ " in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# r) Happy_Prelude.++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# nt) Happy_Prelude.++ ", len: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# len) Happy_Prelude.++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(Happy_Prelude.show (Happy_GHC_Exts.I# new_st) Happy_Prelude.++ ".\n")
  (HappyCons new_st sts1)

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", input " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ ", " Happy_Prelude.++ Happy_Prelude.show (happyDecodeAction act) Happy_Prelude.++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
