{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Hh200.Cli

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import Network.HTTP.Client

import Hh200.Types

import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = cli
