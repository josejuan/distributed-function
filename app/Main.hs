{-# LANGUAGE TemplateHaskell, DeriveGeneric, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Data.Typeable
import GHC.Generics
import Data.Binary
import System.Environment (getArgs)
import Control.Distributed.Process hiding (newChan)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM_, forM)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Distributed.Process.Serializable


data Computation a r = Request  {requestor  :: ProcessId, argument :: a}
                     | Response {respondent :: NodeId, result :: r}
                       deriving (Typeable, Generic)

data Ret a = Ret a | End

instance (Binary a, Binary r) =>Binary (Computation a r)

mkRemoteComputation :: Serializable (Computation a r) =>(a ->r) ->Process ()
mkRemoteComputation f = do
    nid <-getSelfNode
    (req :: Computation a r) <-expect
    case req of
      Request qid a ->send qid (Response nid (f a) :: Computation a r)
      _             ->say "`Request` expected"



distributedSolver :: forall a r . (Typeable a, Typeable r, Binary a, Binary r) =>[String] ->RemoteTable ->Closure (Process ()) ->[a] ->IO [r]
distributedSolver args rTable rComputation instances = do
  let [mode, host, port] = args
  backend <-initializeBackend host port rTable
  case mode of
    "master" ->do
                  ch <-newChan
                  mid <-forkIO $ startMaster backend (prepareMaster ch backend instances)
                  readRet ch
    "slave"  ->do
                  startSlave  backend
                  return []

  where readRet :: Chan (Ret r) ->IO [r]
        readRet ch = do
          r <-readChan ch
          case r of
            Ret x ->(x:) <$> readRet ch
            End   ->return []

        prepareMaster :: Chan (Ret r) ->Backend ->[a] ->[NodeId] ->Process ()
        prepareMaster ch backend instances peers = do
          liftIO $ putStrLn $ "workers: " ++ show peers
          master ch backend peers 0 instances          -- los usamos para resolver el problema

        master :: Chan (Ret r) ->Backend ->[NodeId] ->Int ->[a] ->Process ()
        master ch backend _      0     [] = do
                                              -- terminateAllSlaves backend              -- nada por enviar, nada por recibir
                                              liftIO $ putStrLn "fin: nada por enviar, nada por recibir"
                                              liftIO $ writeChan ch End
        master ch backend []     0  (_:_) = do
                                              liftIO $ putStrLn "fin: hay por enviar pero no hay workers"
                                              say "No available peers!"               -- cosas por enviar pero no hay peers
                                              liftIO $ writeChan ch End
        master ch backend (p:ps) n (x:xs) = do                                      -- falta por enviar y podemos, pues enviamos
                                              liftIO $ putStrLn "enviamos datos a un worker iniciando un nuevo proceso en él"
                                              lpid <-getSelfPid
                                              rpid <-spawn p rComputation     -- creamos procesos de servicio en cada worker
                                              send rpid (Request lpid x :: Computation a r)                -- le pedimos que los resuelva
                                              master ch backend ps (n + 1) xs       -- queda uno menos por enviar y uno más por recibir
        master ch backend []     n     xs = do                                      -- quizás por enviar pero no hay peers disponibles
                                              liftIO $ putStrLn "esperamos datos de algún worker"
                                              (Response i r :: Computation a r) <-expect
                                              liftIO $ putStrLn "recibido"
                                              liftIO $ writeChan ch $ Ret r
                                              master ch backend [i] (n - 1) xs




isPrime :: Integer ->Bool
isPrime n = and [n `mod` r /= 0 | r <-[2..ceiling (sqrt $ fromIntegral n)]]

remoteIsPrime :: Process ()
remoteIsPrime = mkRemoteComputation isPrime

$(remotable ['remoteIsPrime])

remoteTable :: RemoteTable
remoteTable = Main.__remoteTable initRemoteTable

main = do
  args <-getArgs
  (rs :: [Bool]) <-distributedSolver args remoteTable $(mkStaticClosure 'remoteIsPrime) [15485867, 32452867 :: Integer]
  print rs

