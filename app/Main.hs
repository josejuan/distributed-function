{-# LANGUAGE TemplateHaskell, DeriveGeneric, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Control.Distributed.Process.SimpleMap
import System.Environment (getArgs)

isPrime :: Integer ->Bool
isPrime n = and [n `mod` r /= 0 | r <-[2..n-1]] -- complex function...

remoteIsPrime :: Process ()
remoteIsPrime = mkRemoteComputation isPrime
$(remotable ['remoteIsPrime])

remoteTable :: RemoteTable
remoteTable = Main.__remoteTable initRemoteTable

main = do
  (mode: host: port: primes) <-getArgs
  (rs :: [Bool]) <-distributedMap (read mode) host port remoteTable
                   $(mkStaticClosure 'remoteIsPrime) (read <$> primes :: [Integer])
  print rs

