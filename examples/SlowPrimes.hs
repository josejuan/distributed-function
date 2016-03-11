{-# LANGUAGE TemplateHaskell, DeriveGeneric, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Control.Distributed.Process.SimpleMap
import System.Environment (getArgs)

-- complex function to distribute over the network
isPrime :: Integer ->Bool
isPrime n = and [n `mod` r /= 0 | r <-[2..n-1]]

-- for each distributable function
remoteIsPrime :: Process ()
remoteIsPrime = mkRemoteComputation isPrime
$(remotable ['remoteIsPrime])

-- once per module
remoteTable :: RemoteTable
remoteTable = Main.__remoteTable initRemoteTable

-- solving instances with all available nodes over the network
main = do
  (mode: host: port: primes) <-getArgs
  (rs :: [Bool]) <-distributedMap (read mode) host port remoteTable
                   $(mkStaticClosure 'remoteIsPrime) (read <$> primes :: [Integer])
  print rs

