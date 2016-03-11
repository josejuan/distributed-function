# distributed-function

Like a distributed `map` with zero configuration.

# abstract

The idea is set up slave daemons on each machine over the network. After it, use as needed.

# simple example

    {-# LANGUAGE TemplateHaskell, DeriveGeneric, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
    module SlowPrimes where

    import Control.Distributed.Process.SimpleMap
    import System.Environment (getArgs)

    -- complex function to distribute over the network
    isPrime :: ℤ → 𝔹
    isPrime n = and [n `mod` r ≢ 0 | r ← [2…n-1]]

    -- for each distributable function
    remoteIsPrime :: Process ()
    remoteIsPrime = mkRemoteComputation isPrime
    $(remotable ['remoteIsPrime])

    -- once per module
    remoteTable :: RemoteTable
    remoteTable = SlowPrimes.__remoteTable initRemoteTable

    -- solving instances with all available nodes over the network
    main = do
      (mode: host: port: primes) ← getArgs
      (rs :: [𝔹]) ← distributedMap (read mode) host port remoteTable
                       $(mkStaticClosure 'remoteIsPrime) (read ↥ primes :: [ℤ])
      print rs

# simple setup

## running slave nodes

The easy way is simply run n-times

    ./slow-primes-exe <real local ip> <any port>

or you can use `slavesUp` to run one node for each processor unit

    ./slavesUp ./binaries/slow-primes-exe

## stopping slave nodes

    ./slavesDown slow-primes-exe

## running slave nodes on a fresh machine

    ./remoteUp remote-machine ./binaries/slow-primes-exe

## stopping remove slave nodes

    ./remoteDown remote-machine slow-primes-exe
