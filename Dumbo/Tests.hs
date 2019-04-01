#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --ghc-options -threaded --package typed-process --package directory --package filepath --package tasty --package tasty-golden --package dhall --package text

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import Dhall

main :: IO ()
main = bracket
  (startProcess "stack exec Dumbo -- --port=8888 --cacheDir=testCache --tmpDir=testTmp")
  (\dumbo -> putStrLn "Stopping dumbo">>stopProcess dumbo>>putStrLn "Dumbo done")
  (\_ -> do
     threadDelay 100000 
     de <- doesDirectoryExist "testCache" 
     when de $ removeDirectoryRecursive "testCache"
     defaultMain =<< goldenTests)

goldenTests :: IO TestTree
goldenTests = do
  curlFiles <- findByExtension [".curl"] "testcases"
  curlTests <- concat <$> traverse openTests curlFiles
  print (length curlTests)
  return $ testGroup
    "Dumbo golden tests"
    [ goldenVsString (name curlTest)     -- test name
      response            -- golden file path
      (process curlTest)  -- action whose result is tested
    | curlTest <- curlTests
    , let response = "testcases/"++name curlTest ++ ".response"
    ]
 where
  openTests :: FilePath -> IO [CurlCase]
  openTests curlFile = do
    let dexp = T.concat ["./", T.pack curlFile]
    tcs <- input auto dexp `catch` (\(e::SomeException) -> (\x->[x]) <$> input auto dexp)
                           `catch` (\(e::SomeException) -> error (show e))
    return tcs


  process curlTest = do
    (out, err) <- readProcess_
      ( proc
        "curl"
        [ "--silent"
        , "--data"
        , body curlTest
        , "localhost:8888/" ++ route curlTest
        ]
      )
    return (out)

data CurlCase = CC {route::String, body::String,name::String} deriving (Show,Generic)
instance Interpret CurlCase
