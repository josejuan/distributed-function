{-# LANGUAGE DeriveGeneric, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Control.Distributed.Process.SimpleMap (
  mkRemoteComputation
, distributedMap
, NodeType (..)

-- reexport
, Process
, remotable
, RemoteTable
, initRemoteTable
, mkStaticClosure
) where

import Data.Typeable
import GHC.Generics
import Data.Binary (Binary)
import Control.Distributed.Process hiding (newChan)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent.Chan
import Control.Concurrent
import Network.Socket (HostName, ServiceName)

data NodeType = Master | Slave deriving (Read, Show)

data Computation a r = Request  {requestor  :: ProcessId, argument :: a}
                     | Response {respondent :: NodeId, result :: r}
                       deriving (Typeable, Generic)

instance (Binary a, Binary r) =>Binary (Computation a r)

mkRemoteComputation :: forall a r . (Typeable a, Typeable r, Binary a, Binary r) =>(a ->r) ->Process ()
mkRemoteComputation f = do
    nid <-getSelfNode
    (req :: Computation a r) <-expect
    case req of
      Request  qid a ->send qid (Response nid (f a) :: Computation a r)
      Response rid _ ->say $ "`Request` expected by " ++ show nid ++ " but received `Response` from " ++ show rid

distributedMap :: forall a r . (Show a, Show r, Typeable a, Typeable r, Binary a, Binary r) =>
                  NodeType ->HostName ->ServiceName ->RemoteTable ->Closure (Process ()) ->[a] ->IO [r]
distributedMap mode host port rTable rComputation instances = do
  backend <-initializeBackend host port rTable
  case mode of
    Master ->do
                  ch <-newChan
                  mid <-forkIO $ startMaster backend (prepareMaster ch backend instances)
                  readRet ch
    Slave  ->do
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
          say $ show (length instances) ++ " instances"
          say "Peer list:"
          mapM_ (say . ("  " ++) . show) peers
          master ch backend peers 0 instances

        master :: Chan (Ret r) ->Backend ->[NodeId] ->Int ->[a] ->Process ()
        master ch backend _      0     [] = liftIO $ writeChan ch End -- terminateAllSlaves backend
        master ch backend []     0  (_:_) = do
                                              say "No available peers!"
                                              liftIO $ writeChan ch End
        master ch backend (p:ps) n (x:xs) = do
                                              lpid <-getSelfPid
                                              rpid <-spawn p rComputation
                                              say $ "sending " ++ show x ++ " to " ++ show p
                                              send rpid (Request lpid x :: Computation a r)
                                              master ch backend ps (n + 1) xs
        master ch backend ps     n     xs = do
                                              (Response i r :: Computation a r) <-expect
                                              say $ "received " ++ show r ++ " from " ++ show i
                                              liftIO $ writeChan ch $ Ret r
                                              master ch backend (i:ps) (n - 1) xs

data Ret a = Ret a | End


