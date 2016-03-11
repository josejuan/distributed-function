# distributed-function

Like a distributed `map` with zero configuration.

The type is (more or less...)

    distributedMap :: Config → (Problem → Solution) → [Problem] → [Solution]

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

## running / stopping nodes on a remote fresh machine

(no remote previous configuration is needed and only temporary files will be created)

### running slave nodes on a fresh machine

    ./remoteUp remote-machine ./binaries/slow-primes-exe

### stopping remove slave nodes

    ./remoteDown remote-machine slow-primes-exe

# example

(Comments with `<-- `)
Using previous code and scripts setup 4 fresh machines

    $ for r in pepinillo manzanita rugoso 192.168.54.12; do echo "== $r ====="; ./remoteUp $r slow-primes-exe 15000; done
    == pepinillo =====
    WARNING: no device configured, using eth0
    Starting up slave node on 192.168.54.28:15001 ...     <-- 4 CPU
    Starting up slave node on 192.168.54.28:15002 ...
    Starting up slave node on 192.168.54.28:15003 ...
    Starting up slave node on 192.168.54.28:15004 ...
    done
    == manzanita =====
    WARNING: no device configured, using eth0
    Starting up slave node on 192.168.54.26:15001 ...      <-- 4 CPU
    Starting up slave node on 192.168.54.26:15002 ...
    Starting up slave node on 192.168.54.26:15003 ...
    Starting up slave node on 192.168.54.26:15004 ...
    done
    == rugoso =====
    WARNING: no device configured, using enp0s10f0
    Starting up slave node on 192.168.54.23:15001 ...      <-- 4 CPU
    Starting up slave node on 192.168.54.23:15002 ...
    Starting up slave node on 192.168.54.23:15003 ...
    Starting up slave node on 192.168.54.23:15004 ...
    done
    == 192.168.54.12 =====
    WARNING: no device configured, using eth0
    Starting up slave node on 192.168.54.12:15001 ...      <-- 8 CPU
    Starting up slave node on 192.168.54.12:15002 ...
    Starting up slave node on 192.168.54.12:15003 ...
    Starting up slave node on 192.168.54.12:15004 ...
    Starting up slave node on 192.168.54.12:15005 ...
    Starting up slave node on 192.168.54.12:15006 ...
    Starting up slave node on 192.168.54.12:15007 ...
    Starting up slave node on 192.168.54.12:15008 ...
    done

now, run master process

    $ time ./slow-primes-exe Master 192.168.54.28 15000 6385343 ... <-- big prime list
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10: 128 instances <-- prime numbers
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10: Peer list:
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15002:0 <-- available workers
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15003:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15001:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15004:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15006:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15005:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15008:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.12:15007:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.23:15001:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.23:15002:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.23:15003:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15002:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15001:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.23:15004:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15003:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15004:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.28:15001:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.28:15002:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.28:15003:0
    Fri Mar 11 14:17:54 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.28:15004:0
    Fri Mar 11 14:17:55 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385343 to nid://192.168.54.12:15002:0
    Fri Mar 11 14:17:55 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385369 to nid://192.168.54.12:15003:0
    Fri Mar 11 14:17:55 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385391 to nid://192.168.54.12:15001:0
    Fri Mar 11 14:17:55 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385397 to nid://192.168.54.12:15004:0
    ...
    [True,True,True,...]
    real    1m8.025s
    user    0m3.517s
    sys     0m0.807s

stop all slaves

    $ for r in pepinillo manzanita rugoso 192.168.54.12; do ./remoteDown $r slow-primes-exe ; done
    slavesDown
    slavesDown
    slavesDown
    slavesDown

and use only `manzanita` machine

    $ ./remoteUp manzanita slow-primes-exe 15000
    WARNING: no device configured, using eth0
    Starting up slave node on 192.168.54.26:15001 ...
    Starting up slave node on 192.168.54.26:15002 ...
    Starting up slave node on 192.168.54.26:15003 ...
    Starting up slave node on 192.168.54.26:15004 ...
    done

same work

    $ time ./slow-primes-exe Master 192.168.54.28 15000 6385343 ... <-- big prime list
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10: 128 instances
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10: Peer list:
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15001:0
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15002:0
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15004:0
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10:   nid://192.168.54.26:15003:0
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385343 to nid://192.168.54.26:15001:0
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385369 to nid://192.168.54.26:15002:0
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385391 to nid://192.168.54.26:15004:0
    Fri Mar 11 14:22:23 UTC 2016 pid://192.168.54.28:15000:0:10: sending 6385397 to nid://192.168.54.26:15003:0
    Fri Mar 11 14:22:29 UTC 2016 pid://192.168.54.28:15000:0:10: received True from nid://192.168.54.26:15002:0
    ...
    [True,True,True,...]
    real    3m21.445s
    user    0m1.831s
    sys     0m0.536s

much more time.
