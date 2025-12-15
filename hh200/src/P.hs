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
module P where

import Debug.Trace

import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Control.Monad.Trans.Except
import           L
import           Hh200.Types
import Network.HTTP.Types.Status
import qualified BEL
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

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t18 t20 t21
        = HappyTerminal (Token)
        | HappyErrorToken Happy_Prelude.Int
        | HappyAbsSyn5 t5
        | HappyAbsSyn6 t6
        | HappyAbsSyn7 t7
        | HappyAbsSyn8 t8
        | HappyAbsSyn9 t9
        | HappyAbsSyn10 t10
        | HappyAbsSyn11 t11
        | HappyAbsSyn12 t12
        | HappyAbsSyn13 t13
        | HappyAbsSyn14 (RhsDict)
        | HappyAbsSyn16 (Binding)
        | HappyAbsSyn17 ([String])
        | HappyAbsSyn18 t18
        | HappyAbsSyn19 ([Int])
        | HappyAbsSyn20 t20
        | HappyAbsSyn21 t21

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["d","identifier","newline","\"=\"","\"/\"","\".\"","\",\"","\":\"","\"\\\"\"","\"{\"","\"}\"","\"(\"","\")\"","\"[\"","\"]\"","\"then\"","\"HTTP\"","\"Configs\"","\"Captures\"","\"Asserts\"","method","url","s","braced","rhs","line","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe7\xff\xff\xff\xfe\xff\xff\xff\xf8\xff\xff\xff\x21\x00\x00\x00\x1a\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\xf8\xff\xff\xff\x00\x00\x00\x00\x10\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x4e\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\xf8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x0e\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x3c\x00\x00\x00\x46\x00\x00\x00\x53\x00\x00\x00\x55\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x56\x00\x00\x00\x49\x00\x00\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x57\x00\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x5a\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xff\xff\x5a\x00\x00\x00\x5a\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x00\x00\x5b\x00\x00\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x41\x00\x00\x00\x5c\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x20\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x62\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x3e\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x47\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x77\x00\x00\x00\x00\x00\x00\x00\x78\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x7a\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\xff\xff\xfb\xff\xff\xff\xfd\xff\xff\xff\xfa\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xff\xff\xf3\xff\xff\xff\xed\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\xd9\xff\xff\xff\xfb\xff\xff\xff\xd8\xff\xff\xff\xda\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xff\xff\xdb\xff\xff\xff\xfe\xff\xff\xff\xdc\xff\xff\xff\xf5\xff\xff\xff\xfb\xff\xff\xff\xde\xff\xff\xff\x00\x00\x00\x00\xe7\xff\xff\xff\xd7\xff\xff\xff\xfb\xff\xff\xff\xee\xff\xff\xff\xfb\xff\xff\xff\xe5\xff\xff\xff\x00\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\xfb\xff\xff\xff\xe9\xff\xff\xff\xdd\xff\xff\xff\xfb\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xff\xff\xf9\xff\xff\xff\xf8\xff\xff\xff\x00\x00\x00\x00\xef\xff\xff\xff\x00\x00\x00\x00\xfb\xff\xff\xff\xf0\xff\xff\xff\xe4\xff\xff\xff\xfb\xff\xff\xff\xf1\xff\xff\xff\xe2\xff\xff\xff\xfb\xff\xff\xff\xfb\xff\xff\xff\xf7\xff\xff\xff\xfb\xff\xff\xff\xe6\xff\xff\xff\xea\xff\xff\xff\xeb\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\xf6\xff\xff\xff\xe3\xff\xff\xff\xe1\xff\xff\xff\xfb\xff\xff\xff\xec\xff\xff\xff\xe0\xff\xff\xff\xdf\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x04\x00\x00\x00\x04\x00\x00\x00\x1c\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x0f\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x0b\x00\x00\x00\x04\x00\x00\x00\x0f\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x0f\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x19\x00\x00\x00\x02\x00\x00\x00\x19\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x0f\x00\x00\x00\x04\x00\x00\x00\x13\x00\x00\x00\x12\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x04\x00\x00\x00\x11\x00\x00\x00\x17\x00\x00\x00\x0f\x00\x00\x00\x1c\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x18\x00\x00\x00\x09\x00\x00\x00\x12\x00\x00\x00\x0f\x00\x00\x00\x0c\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x04\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x04\x00\x00\x00\x14\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x15\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\x01\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\x01\x00\x00\x00\xff\xff\xff\xff\x01\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0c\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\xff\xff\xff\xff\x2a\x00\x00\x00\x0c\x00\x00\x00\x2a\x00\x00\x00\x3d\x00\x00\x00\x3c\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x13\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x3e\x00\x00\x00\x0c\x00\x00\x00\x14\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x0d\x00\x00\x00\x4f\x00\x00\x00\x3d\x00\x00\x00\x2b\x00\x00\x00\x05\x00\x00\x00\x40\x00\x00\x00\x2e\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x19\x00\x00\x00\x0c\x00\x00\x00\x13\x00\x00\x00\x1a\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x13\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0c\x00\x00\x00\x1b\x00\x00\x00\x12\x00\x00\x00\x34\x00\x00\x00\xd9\xff\xff\xff\x09\x00\x00\x00\x1d\x00\x00\x00\x19\x00\x00\x00\x1c\x00\x00\x00\x31\x00\x00\x00\x1a\x00\x00\x00\x2e\x00\x00\x00\x32\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\xf4\xff\xff\xff\xf4\xff\xff\xff\x3a\x00\x00\x00\x0c\x00\x00\x00\x23\x00\x00\x00\x36\x00\x00\x00\x2a\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x1e\x00\x00\x00\x17\x00\x00\x00\x0c\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x46\x00\x00\x00\x28\x00\x00\x00\x0c\x00\x00\x00\x26\x00\x00\x00\x22\x00\x00\x00\x20\x00\x00\x00\x31\x00\x00\x00\x0c\x00\x00\x00\x23\x00\x00\x00\x2f\x00\x00\x00\x46\x00\x00\x00\x0c\x00\x00\x00\x3a\x00\x00\x00\x36\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x2a\x00\x00\x00\x02\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x15\x00\x00\x00\x1c\x00\x00\x00\x10\x00\x00\x00\x52\x00\x00\x00\x26\x00\x00\x00\x24\x00\x00\x00\x23\x00\x00\x00\x2f\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x20\x00\x00\x00\x3a\x00\x00\x00\x36\x00\x00\x00\x34\x00\x00\x00\x48\x00\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x40\x00\x00\x00\x44\x00\x00\x00\x4c\x00\x00\x00\x4b\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 40) [
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
        (40 , happyReduce_40)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\x07\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x07\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x05\x00\x00\x00\x08\x00\x00\x00\x05\x00\x00\x00\x08\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x08\x00\x00\x00\x02\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x04\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 29 :: Happy_Prelude.Int
happy_n_nonterms = 17 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 (HappyAbsSyn21  happy_var_3)
        (HappyAbsSyn7  happy_var_2)
        _
         =  HappyAbsSyn5
                 (Script { kind = Regular, config = happy_var_2, callItems = happy_var_3 }
        )
happyReduction_1 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_2  0# happyReduction_2
happyReduction_2 (HappyAbsSyn21  happy_var_2)
        _
         =  HappyAbsSyn5
                 (Script { kind = Regular, config = defaultScriptConfig, callItems = happy_var_2 }
        )
happyReduction_2 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_2  0# happyReduction_3
happyReduction_3 _
        _
         =  HappyAbsSyn5
                 (Script { kind = Regular, config = dbgScriptConfig, callItems = [] }
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_0  1# happyReduction_4
happyReduction_4  =  HappyAbsSyn6
                 (
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_2  1# happyReduction_5
happyReduction_5 _
        _
         =  HappyAbsSyn6
                 (
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happyReduce 5# 2# happyReduction_6
happyReduction_6 ((HappyAbsSyn8  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn7
                 (foldl (\cfg f -> f cfg) defaultScriptConfig happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happySpecReduce_1  3# happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn8
                 ([happy_var_1]
        )
happyReduction_7 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_2  3# happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn8
                 (happy_var_1 ++ [happy_var_2]
        )
happyReduction_8 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  4# happyReduction_9
happyReduction_9 _
        (HappyTerminal (RHS _ happy_var_2))
        (HappyTerminal (IDENTIFIER _ happy_var_1))
         =  HappyAbsSyn9
                 (\c -> case (happy_var_1, stripColon happy_var_2) of
        ("use-tls", "false") -> c { useTls = Just False }
        ("use-tls", "true")  -> c { useTls = Just True }
        _ -> trace ("Unknown config: " ++ happy_var_1) c
        )
happyReduction_9 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_3  5# happyReduction_10
happyReduction_10 (HappyTerminal (QUOTED _ happy_var_3))
        _
        (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn10
                 (DepsClause { deps = happy_var_1, itemName = happy_var_3 }
        )
happyReduction_10 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_1  5# happyReduction_11
happyReduction_11 (HappyTerminal (QUOTED _ happy_var_1))
         =  HappyAbsSyn10
                 (DepsClause { deps = [], itemName = happy_var_1 }
        )
happyReduction_11 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happySpecReduce_1  6# happyReduction_12
happyReduction_12 (HappyTerminal (QUOTED _ happy_var_1))
         =  HappyAbsSyn11
                 ([happy_var_1]
        )
happyReduction_12 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_2  6# happyReduction_13
happyReduction_13 (HappyTerminal (QUOTED _ happy_var_2))
        (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn11
                 (happy_var_1 ++ [happy_var_2]
        )
happyReduction_13 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happyReduce 6# 7# happyReduction_14
happyReduction_14 (_ `HappyStk`
        (HappyTerminal (BRACED _ happy_var_5)) `HappyStk`
        (HappyAbsSyn14  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (URL _ happy_var_2)) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSpec { verb = expectUpper    happy_var_1, url = happy_var_2, headers = happy_var_4, payload = happy_var_5, opts = [] }
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happyReduce 5# 7# happyReduction_15
happyReduction_15 (_ `HappyStk`
        (HappyAbsSyn14  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (URL _ happy_var_2)) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSpec { verb = expectUpper    happy_var_1, url = happy_var_2, headers = happy_var_4, payload = "", opts = [] }
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happyReduce 5# 7# happyReduction_16
happyReduction_16 (_ `HappyStk`
        (HappyTerminal (BRACED _ happy_var_4)) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (URL _ happy_var_2)) `HappyStk`
        (HappyTerminal (METHOD _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (RequestSpec { verb = expectUpper    happy_var_1, url = happy_var_2, headers = RhsDict HM.empty, payload = happy_var_4, opts = [] }
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_3  7# happyReduction_17
happyReduction_17 _
        (HappyTerminal (URL _ happy_var_2))
        (HappyTerminal (METHOD _ happy_var_1))
         =  HappyAbsSyn12
                 (RequestSpec { verb = expectUpper    happy_var_1, url = happy_var_2, headers = RhsDict HM.empty, payload = "", opts = [] }
        )
happyReduction_17 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_2  7# happyReduction_18
happyReduction_18 _
        (HappyTerminal (URL _ happy_var_1))
         =  HappyAbsSyn12
                 (RequestSpec { verb = expectUpper "GET", url = happy_var_1, headers = RhsDict HM.empty, payload = "", opts = [] }
        )
happyReduction_18 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happyReduce 7# 8# happyReduction_19
happyReduction_19 (_ `HappyStk`
        (HappyAbsSyn17  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn14  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn13
                 (trace "RSa" $ ResponseSpec { asserts = happy_var_6, captures = happy_var_4, output = [], statuses = map statusFrom happy_var_2 }
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happyReduce 5# 8# happyReduction_20
happyReduction_20 (_ `HappyStk`
        (HappyAbsSyn14  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn13
                 (trace "RSb" $ ResponseSpec { asserts = [], captures = happy_var_4, output = [], statuses = map statusFrom happy_var_2 }
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happyReduce 5# 8# happyReduction_21
happyReduction_21 (_ `HappyStk`
        (HappyAbsSyn17  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn13
                 (trace "RSc" $ ResponseSpec { asserts = happy_var_4, captures = RhsDict HM.empty, output = [], statuses = map statusFrom happy_var_2 }
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_3  8# happyReduction_22
happyReduction_22 _
        (HappyAbsSyn19  happy_var_2)
        _
         =  HappyAbsSyn13
                 (trace "RSd" $ ResponseSpec { asserts = [], captures = RhsDict HM.empty, output = [], statuses = map statusFrom happy_var_2 }
        )
happyReduction_22 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happyReduce 4# 8# happyReduction_23
happyReduction_23 (_ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn14  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn13
                 (trace "RSe" $ ResponseSpec { asserts = [], captures = happy_var_1, output = [], statuses = [] }
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_2  8# happyReduction_24
happyReduction_24 _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn13
                 (trace "RSf" $ ResponseSpec { asserts = [], captures = happy_var_1, output = [], statuses = [] }
        )
happyReduction_24 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happyReduce 5# 9# happyReduction_25
happyReduction_25 ((HappyAbsSyn14  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happySpecReduce_1  10# happyReduction_26
happyReduction_26 (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn14
                 (RhsDict (HM.fromList [happy_var_1])
        )
happyReduction_26 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_2  10# happyReduction_27
happyReduction_27 (HappyAbsSyn16  happy_var_2)
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (let (RhsDict acc) = happy_var_1 in
                              RhsDict (HM.insertWith (++) (fst happy_var_2) (snd happy_var_2) acc)
        )
happyReduction_27 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happyReduce 4# 11# happyReduction_28
happyReduction_28 (_ `HappyStk`
        (HappyTerminal (QUOTED _ happy_var_3)) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (IDENTIFIER _ happy_var_1)) `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 ((happy_var_1, [BEL.R (Text.pack happy_var_3)])
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happySpecReduce_3  11# happyReduction_29
happyReduction_29 _
        (HappyTerminal (RHS _ happy_var_2))
        (HappyTerminal (IDENTIFIER _ happy_var_1))
         =  HappyAbsSyn16
                 ((happy_var_1, [BEL.L (Text.pack (drop 1 happy_var_2))])
        )
happyReduction_29 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happyReduce 5# 12# happyReduction_30
happyReduction_30 ((HappyAbsSyn18  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn17
                 (happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happySpecReduce_2  13# happyReduction_31
happyReduction_31 _
        (HappyTerminal (LINE _ happy_var_1))
         =  HappyAbsSyn18
                 ([happy_var_1]
        )
happyReduction_31 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happySpecReduce_2  13# happyReduction_32
happyReduction_32 (HappyTerminal (LINE _ happy_var_2))
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 ((happy_var_1 ++ [happy_var_2])
        )
happyReduction_32 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happySpecReduce_1  14# happyReduction_33
happyReduction_33 (HappyTerminal (DIGITS _ happy_var_1))
         =  HappyAbsSyn19
                 ([read happy_var_1]
        )
happyReduction_33 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_2  14# happyReduction_34
happyReduction_34 (HappyTerminal (DIGITS _ happy_var_2))
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn19
                 (happy_var_1 ++ [read happy_var_2]
        )
happyReduction_34 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happySpecReduce_3  15# happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_3)
        (HappyAbsSyn12  happy_var_2)
        (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn20
                 (trace "call_itemA" (pCallItem happy_var_1 happy_var_2 (Just happy_var_3))
        )
happyReduction_35 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_36 = happySpecReduce_2  15# happyReduction_36
happyReduction_36 (HappyAbsSyn12  happy_var_2)
        (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn20
                 (trace "call_itemB" (pCallItem happy_var_1 happy_var_2 Nothing)
        )
happyReduction_36 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_37 = happySpecReduce_2  15# happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_2)
        (HappyAbsSyn12  happy_var_1)
         =  HappyAbsSyn20
                 (trace ("call_itemC: " ++ show happy_var_2) pCallItem defaultDepsClause happy_var_1 (Just happy_var_2)
        )
happyReduction_37 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_38 = happySpecReduce_1  15# happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_1)
         =  HappyAbsSyn20
                 (trace "call_itemD" (pCallItem defaultDepsClause happy_var_1 Nothing)
        )
happyReduction_38 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_39 = happySpecReduce_2  16# happyReduction_39
happyReduction_39 _
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn21
                 ([happy_var_1]
        )
happyReduction_39 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_40 = happySpecReduce_3  16# happyReduction_40
happyReduction_40 _
        (HappyAbsSyn20  happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn21
                 (happy_var_1 ++ [happy_var_2]
        )
happyReduction_40 _ _ _  = notHappyAtAll 

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
        KW_CAPTURES _ -> 20#;
        KW_ASSERTS _ -> 21#;
        METHOD _ happy_dollar_dollar -> 22#;
        URL _ happy_dollar_dollar -> 23#;
        QUOTED _ happy_dollar_dollar -> 24#;
        BRACED _ happy_dollar_dollar -> 25#;
        RHS _ happy_dollar_dollar -> 26#;
        LINE _ happy_dollar_dollar -> 27#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 28# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 28# tk explist resume tks = happyReport' tks explist resume
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


-- getLineNum :: Token -> Int
-- getLineNum (Token (AlexPn _ line _) _) = line
-- traceWithLine :: Token -> String -> a -> a
-- traceWithLine tok msg val = trace (msg ++ " at line " ++ show (getLineNum tok)) val

statusFrom :: Int -> Status
statusFrom n = mkStatus n ""

stripColon :: String -> String
stripColon s = dropWhile (\c -> c == ':' || c == ' ') s

parseError :: [Token] -> E a
parseError tokens = failE $ "Parse error on tokens: " ++ show tokens

data E a = ParseOk a | ParseFailed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       ParseOk a     -> k a
       ParseFailed e -> ParseFailed e

returnE :: a -> E a
returnE a = ParseOk a

failE :: String -> E a
failE err = ParseFailed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      ParseOk a     -> ParseOk a
      ParseFailed e -> k e
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
