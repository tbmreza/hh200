-- module Hh200.Etf (wr) where
module Hh200.Etf where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Word
import Data.Int
import Data.Bits
import System.IO

-- https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#introduction
tag_NEW_FLOAT      = 70 :: Word8  -- 'F'
tag_BIT_BINARY     = 77 :: Word8  -- 'M'
tag_COMPRESSED     = 80 :: Word8  -- 'P'
tag_SMALL_INTEGER  = 97 :: Word8  -- 'a'
tag_INTEGER        = 98 :: Word8  -- 'b'
tag_FLOAT          = 99 :: Word8  -- 'c'
tag_ATOM           = 100 :: Word8 -- 'd'
tag_REFERENCE      = 101 :: Word8 -- 'e'
tag_PORT           = 102 :: Word8 -- 'f'
tag_PID            = 103 :: Word8 -- 'g'
tag_SMALL_TUPLE    = 104 :: Word8 -- 'h'
tag_LARGE_TUPLE    = 105 :: Word8 -- 'i'
tag_NIL            = 106 :: Word8 -- 'j'
tag_STRING         = 107 :: Word8 -- 'k'
tag_LIST           = 108 :: Word8 -- 'l'
tag_BINARY         = 109 :: Word8 -- 'm'
tag_SMALL_BIG      = 110 :: Word8 -- 'n'
tag_LARGE_BIG      = 111 :: Word8 -- 'o'
tag_ATOM_UTF8      = 118 :: Word8 -- 'v'
tag_SMALL_ATOM_UTF8 = 119 :: Word8 -- 'w'

formatVersion = 131 :: Word8

termToBinary :: Term -> BS.ByteString
termToBinary term = 
    BS.toStrict $ B.toLazyByteString $ B.word8 formatVersion <> encodeTermBuilder term


-- Erlang term types
data Term =
    AtomTerm    String
  | IntegerTerm Int
  | FloatTerm   Double
  | BinaryTerm  BS.ByteString
  | ListTerm    [Term]
  | TupleTerm   [Term]
  deriving (Show, Eq)


-- ByteString builders
encodeTermBuilder :: Term -> B.Builder
encodeTermBuilder (AtomTerm str) = 
    let bytes = BS.pack $ map (fromIntegral . fromEnum) str
        len = BS.length bytes in
    if len <= 255
        then B.word8 tag_SMALL_ATOM_UTF8 <> B.word8 (fromIntegral len) <> B.byteString bytes
        else error "Atom too long"

encodeTermBuilder (IntegerTerm i)
    | i >= 0 && i < 256 =
        B.word8 tag_SMALL_INTEGER <> B.word8 (fromIntegral i)
    | otherwise =
        B.word8 tag_INTEGER <> B.int32BE (fromIntegral i)

encodeTermBuilder (FloatTerm d) = 
    B.word8 tag_NEW_FLOAT
 <> B.doubleBE d

encodeTermBuilder (BinaryTerm bs) =
    let len = BS.length bs in
    B.word8 tag_BINARY
 <> B.int32BE (fromIntegral len)
 <> B.byteString bs

encodeTermBuilder (ListTerm terms)
    | null terms = B.word8 tag_NIL
    | otherwise =
        B.word8 tag_LIST
     <> B.int32BE (fromIntegral $ length terms)
     <> mconcat (map encodeTermBuilder terms)
     <> B.word8 tag_NIL

encodeTermBuilder (TupleTerm terms) = 
    let len = length terms in
    if len < 256
        then B.word8 tag_SMALL_TUPLE
          <> B.word8 (fromIntegral len)
          <> mconcat (map encodeTermBuilder terms)
        else B.word8 tag_LARGE_TUPLE
          <> B.int32BE (fromIntegral len)
          <> mconcat (map encodeTermBuilder terms)


writeTermToFile :: FilePath -> Term -> IO ()
writeTermToFile filepath term = do
    let binaryData = termToBinary term
    BS.writeFile filepath binaryData
    putStrLn $ "Term written to ./" ++ filepath

asBinaryTerm s = BinaryTerm $ BS.pack $ map (fromIntegral . fromEnum) s

parsed :: Term
parsed =
    ListTerm [
        TupleTerm [AtomTerm "post", asBinaryTerm "http://localhost:9999/413-Content-Too-Large.php"]
      , TupleTerm [AtomTerm "json", TupleTerm [AtomTerm "mut", asBinaryTerm "/home/tbmreza/gh/hh200/building-blocks/rt/asset.json", IntegerTerm 9]]
      , TupleTerm [AtomTerm "probe_valid_size", IntegerTerm 4100, IntegerTerm 200]
      ]
