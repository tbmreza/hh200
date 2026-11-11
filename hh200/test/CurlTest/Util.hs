-- | Shared string helpers for the curl test-suite adapter. Extracted from
-- @CurlTest.Parse@/@.Preprocess@/@.Translate@, which previously carried their
-- own copies.
module CurlTest.Util
    ( trim
    , breakOnString
    , splitOn
    , replace
    , replaceAll
    ) where

import           Data.Char (isSpace)
import           Data.List (isPrefixOf)

-- | Trim leading/trailing whitespace.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Split at the first occurrence of @needle@; the needle is not consumed.
breakOnString :: Eq a => [a] -> [a] -> ([a], [a])
breakOnString needle hay = go hay
  where
    go s
        | needle `isPrefixOf` s = ([], s)
        | otherwise =
            case s of
                [] -> ([], [])
                (c:cs) -> let (a, b) = go cs in (c : a, b)

-- | Split a list on every occurrence of a non-empty delimiter.
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = error "splitOn: empty delimiter"
splitOn delim xs = go xs
  where
    go s
        | null s = []
        | delim `isPrefixOf` s = [] : go (drop (length delim) s)
        | otherwise =
            let (a, b) = breakOnString delim s
            in a : if null b then [] else go (drop (length delim) b)

-- | Replace every occurrence of @from@ with @to@.
replace :: String -> String -> String -> String
replace _ _ [] = []
replace from to s@(c:cs)
    | from `isPrefixOf` s = to ++ replace from to (drop (length from) s)
    | otherwise = c : replace from to cs

-- | Apply a list of @(from, to)@ replacements, in order.
replaceAll :: [(String, String)] -> String -> String
replaceAll pairs s0 = foldl (\s (from, to) -> replace from to s) s0 pairs
