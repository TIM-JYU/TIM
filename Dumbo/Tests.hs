#!/usr/bin/env stack
-- stack --resolver lts-11.7 script --ghc-options -threaded --package typed-process --package directory --package filepath --package tasty --package tasty-golden --package dhall --package text

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -threaded #-}
module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath (takeBaseName, replaceExtension)
import System.Process.Typed
import Control.Exception
import Control.Concurrent
import System.Directory
import Data.Monoid
import qualified Data.Text.Lazy as LT
import Dhall

main :: IO ()
main = bracket
  (startProcess "stack exec Dumbo -- --port=8888 --cacheDir=testCache --tmpDir=testTmp")
  (\dumbo -> putStrLn "Stopping dumbo">>stopProcess dumbo>>putStrLn "Dumbo done")
  (\_ -> threadDelay 100000 >> (defaultMain =<< goldenTests))

goldenTests :: IO TestTree
goldenTests = do
          curlFiles <- findByExtension [".curl"] "testcases"
          return $ testGroup "Dumbo golden tests"
            [ goldenVsString
                (takeBaseName curlFile) -- test name
                responses -- golden file path
                (process curlFile)  -- action whose result is tested
            | curlFile <- curlFiles
            , let responses = replaceExtension curlFile ".response"
            ]
 where 
  process curlFile = do
                      c <- input auto (LT.concat ["./",LT.pack curlFile])
                      (out,err) <- readProcess_ (proc "curl" [ "--silent","--data", body c,"localhost:8888/"++route c])
                      return (out)

data CurlCase = CC {route::String, body::String} deriving (Show,Generic)
instance Interpret CurlCase
