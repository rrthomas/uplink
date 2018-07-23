{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Network.P2P.Service
  ( Service(..)
  , ServiceSpec(..)
  , HList(..)

  , serviceToProc
  , serviceSpecToProc

  , findLocalService
  , waitLocalService
  , waitLocalService'

  , waitLocalProc
  , waitLocalProc'

  , findRemoteService
  , findRemoteServiceTimeout
  ) where

import Protolude hiding (All)

import Control.Distributed.Static (Closure, staticClosure, staticLabel, registerStatic)
import qualified Control.Distributed.Process.Supervisor as DPS
import qualified Control.Distributed.Process.Supervisor.Management as DPSM

import Control.Distributed.Process.Lifted
        ( Process, NodeId, ProcessId, RemoteTable
        , WhereIsReply(..), whereis, whereisRemoteAsync
        , expectTimeout, receiveChan, terminate
        , getSelfPid, register, spawnLocal
        )
import Control.Distributed.Process.Lifted.Class

import Data.Rank1Dynamic (toDynamic)
import qualified Data.Binary as B

import NodeState (NodeT)
import DB (MonadReadWriteDB)

import qualified Network.P2P.Logging as Log

-- This module defines the interface with which an Uplink Node Service is to be
-- defined. Uplink Services will be defined using a "Typed Process Tree", such
-- that each node in the tree is either a Supervisor Process or a Worker
-- Process where the internal nodes of the tree are the supervisors, and the
-- leaves of the tree are the worker processes.
--
-- There are two different specifications for a service:
--
--   > 'Supervisor:
--       A process that spawns and supervises given specific, hard-coded rules
--       (and in the future, specified in arguments to the 'runService' function)
--       that describe the way the child processes should be stopped and
--       started, in what order, etc. To instantiate a supervisor, the user
--       defines an 'HList' of values that implement the 'Service' type class;
--       No explicit process is provided to run, simply a list of child services
--       such that to be supervised by the supervisor process. Supervisors can
--       be children of other supervisors.
--
--   > 'Worker':
--       A "leaf" process in the supervisor tree that has no child processes.
--       The user simply provides a `Process ()` to run as the child process,
--       and the rest is taken care of.
--
-- All well defined services of the Uplink multi-process architecture should be
-- be implemented using this interface. The 'runService' provides robust
-- supervision principles using the `distributed-process-supervisor` library.
-- Within uplink services, `distributed-process` primitives like 'spawnLocal',
-- 'link', and `monitor' can still be used, but provide a more granular and
-- difficult method to accomplish the kind of supervision the supervisor library
-- does. If a new process with substantial responsibilites is to be added to
-- uplink, strongly consider using this modules API as depicted in most
-- `Network.P2P.X` modules within the Uplink codebase.
--
-- This implementation yields a "supervised process tree" defined on the value
-- level by a the use of the 'ServiceSpec' type. A service value wishing to be
-- a part of the tree (or be a supervisor of other services) needs to:
--
--   1) Be a "singleton type", i.e. a type with one value constructor with no
--   fields.
--   2) Implement an instance of the type class 'Service' which simply relates a
--   value to a 'ServiceSpec'.
--
-- The result is such that a service defined of the form:
--
-- ```
-- data SupervisorA = SupervisorA
-- data SupervisorB = SupervisorB
--
-- data WorkerA = WorkerA
-- data WorkerB = WorkerB
-- data WorkerC = WorkerC
--
--
-- instance Service SupervisorA where
--   serviceSpec _ = Supervisor (WorkerA ::: SupervisorB ::: HNil)
--
-- instance Service SupervisorB where
--   serviceSpec _ = Supervisor (WorkerB ::: WorkerC ::: HNil)
--
-- instance Service WorkerA where
--   serviceSpec _ = Worker (Log.info "WorkerA")
--
-- instance Service WorkerB where
--   serviceSpec _ = Worker (Log.info "WorkerB")
--
-- instance Service WorkerC where
--   serviceSpec _ = Worker (Log.info "WorkerC")
--```
--
-- This definition of Supervisors and workers would yield the following process
-- tree:
--
--                         SupervisorA
--                              |
--                +-------------+-------------+
--                |                           |
--             WorkerA                   SupervisorB
--                                            |
--                                 +----------+----------+
--                                 |                     |
--                              WorkerB               WorkerC
--
-- Such that `SupervisorA` is "in charge" (i.e. monitoring) `WorkerA` and
-- `SupervisorB`; However, due to the implementation of
-- `distributed-process-supervisor` `SupervisorA` should only have worry about
-- `WorkerA`. In theory, `SupervisorB` will independently manage `WorkerB` and
-- `WorkerC` such that if there are any faults in these two processes, then
-- `SupervisorA` and `WorkerA` will be unaffected and continue operation as
-- normal (there are special configuration options that can change these
-- semantics, but for the time being this is true).
--
-- Note: This entire API could be implemented at the type level, such that users
-- define a list of types instead of an HList and through use of 'Proxy a` or
-- `undefined :: a`, this api could properly run services without the need of
-- a list of services on the value level. However, reifying this idea would take
-- a bit more effort and time, both which are not of the essence at the moment.

--------------------------------------------------------------------------------
-- Service Abstraction
--------------------------------------------------------------------------------

-- | A class constraining the structure of a type to have no type parameters,
-- and a single constructor with no fields: @data A = A@
class SingletonType (k :: * -> *) where
instance SingletonType (D1 a (C1 b U1))

-- | A type class encapsulating the notion of an Uplink Service being a Worker
-- or a Supervisor. This type class only allows instance definitions for types
-- with a single, parameterless constructor.
class (Show a, B.Binary a, Generic a, SingletonType (Rep a)) => Service a where
  serviceSpec :: forall m. (MonadReadWriteDB m, MonadProcessBase m) => a -> ServiceSpec (NodeT m)

data HList (xs :: [*]) where
  HNil  :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::

-- | A type family to generate constraints that all values in a type level list
-- satisfy some type constraint.
type family All (p :: * -> Constraint) (xs :: [*]) :: Constraint where
  All _ '[]       = ()
  All p (x ': xs) = (p x, All p xs)

data ServiceSpec m where
  Worker :: MonadProcessBase m => m () -> ServiceSpec m
  -- ^ Worker Services are "leaves" of the supervisor process tree; They have
  -- no children of their own and only contain a `Process ()` computation.
  Supervisor :: (All Service xs, MonadProcessBase m) => HList xs -> ServiceSpec m
  -- ^ Supervisor services are internal nodes in the n-ary supervisor tree;
  -- They only supervise other processes, logging 'MxSupervisor' events, and
  -- nothing else.

-- | Convert a Service to a Process and append-to-remote-table function.
serviceToProc
  :: forall m a. (MonadReadWriteDB m, MonadProcessBase m, Service a)
  => (NodeT m () -> Process ()) -- ^ Convert a (NodeT m) computation into a process
  -> a                          -- ^ The service to translate
  -> (Process (), RemoteTable -> RemoteTable)
serviceToProc runInProc s =
  serviceSpecToProc runInProc (show s) (serviceSpec s)

-- | Convert a service spec to a Process and a function that adds itself and all
-- children to an initial remote table.
serviceSpecToProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => (NodeT m () -> Process ()) -- ^ Convert a (NodeT m) computation into a process
  -> [Char]                     -- ^ Name to register the spec as in the remote table
  -> ServiceSpec (NodeT m)      -- ^ The service specification to translate
  -> (Process (), RemoteTable -> RemoteTable)
serviceSpecToProc runInProc name spec =
    case spec of
      Worker p ->
        let proc = runInProc p
         in (proc, snd (mkProcBaseClosure name proc))
      Supervisor cs -> mkSupervisorProc cs
  where
    -- Make a supervisor ServiceSpec value into a process by recursively
    -- generating all the ChildSpec values of it's Child services
    mkSupervisorProc
      :: (All (Service) xs)
      => HList xs -> (Process (), RemoteTable -> RemoteTable)
    mkSupervisorProc services =
        let (childSpecs, mkRemoteTable) = servicesToChildSpecs services
         in (supervisorProc childSpecs, mkRemoteTable)
      where
        supervisorProc childSpecs = do
          -- Spawn the supervisor monitoring agent to listen for
          -- 'MxSupervisor' events on the managed-process msg bus
          spawnLocal (void DPSM.supervisionMonitor)
          -- Register this process as the supervisor service name
          register name =<< getSelfPid
          -- Run the supervisor process and monitor it
          mxSupRp <- DPSM.monitorSupervisor =<<
            DPS.start DPS.restartOne DPS.ParallelShutdown childSpecs
          -- Log all 'MxSupervisor' events from the supervisor monitor
          forever $ Log.info . show =<< receiveChan mxSupRp

        servicesToChildSpecs
          :: (All (Service) xs)
          => HList xs
          -> ([DPS.ChildSpec], RemoteTable -> RemoteTable)
        servicesToChildSpecs = \case
          -- If there are no more services, no childspec or rt entry to gen
          HNil -> ([], identity)
          -- Generate, recursively, the child specs and remote table entries of
          -- every sub-process-tree of the current service s, as well as the
          -- remainder of the services in the list of supervised services
          (s ::: rest) ->
            let (childSpec, mkRemoteTable) = serviceToChildSpec runInProc s
                (childSpecs, mkRemoteTables) = servicesToChildSpecs rest
             in (childSpec : childSpecs, mkRemoteTable . mkRemoteTables)

-- | Convert an Uplink Service into a ChildSpec
serviceToChildSpec
  :: forall m a. (MonadReadWriteDB m, MonadProcessBase m, Service a)
  => (NodeT m () -> Process ())
  -> a
  -> (DPS.ChildSpec, RemoteTable -> RemoteTable)
serviceToChildSpec runInProc s =
  -- Generate a closure and a function to add all child services (which may
  -- include other supervisors) to the node's remote table.
  let (procBaseClosure, mkRemoteTable) = mkServiceClosure runInProc s
   in (,mkRemoteTable) $
     case (serviceSpec s :: ServiceSpec (NodeT m)) of
       Supervisor children -> DPS.ChildSpec
         { childKey          = show s
         , childType         = DPS.Supervisor
         , childRestart      = DPS.Transient
         , childRestartDelay = Nothing
         , childStop         = DPS.StopImmediately
         , childStart        = DPS.RunClosure procBaseClosure
         , childRegName      = Nothing
         }

       Worker proc -> DPS.ChildSpec
         { childKey          = show s
         , childType         = DPS.Worker
         , childRestart      = DPS.Transient
         , childRestartDelay = Nothing
         , childStop         = DPS.StopImmediately
         , childStart        = DPS.RunClosure procBaseClosure
         , childRegName      = Just (DPS.LocalName (show s))
         }

--------------------------------------------------------------------------------
-- Closure and RemoteTable Creation
--------------------------------------------------------------------------------

-- To effectively utilize the `distributed-process-supervisor` API, we must
-- generate a 'Closure' for every process in the Service process tree. This
-- means that the Supervisor library (in its current state) needs to know how to
-- serialize a a process computation and its corresponding environment, and also
-- how to look it up on another node. Even though all uplink services
-- (processes) are never remotely spawned, this is still a constraint for use of
-- the `distributed-process-supervisor` library.
--
-- These functions turn a `MonadProcessBase m => NodeT m ()` into a `Closure
-- (Process ()) value and a function to add it to a remote table.

-- | Turn a 'Service' into a Closure and return the function that adds the
-- closure to a remote table.
mkServiceClosure
  :: forall m s. (MonadProcessBase m, MonadReadWriteDB m, Service s)
  => (NodeT m () -> Process ())
  -> s
  -> (Closure (Process ()), RemoteTable -> RemoteTable)
mkServiceClosure runInProc s =
  let (proc, mkRemoteTables) = serviceToProc runInProc s
      (closure, mkRemoteTable) = mkProcBaseClosure (show s) proc
   in (closure, mkRemoteTable . mkRemoteTables)

-- | Given a name and a 'Process ()' value, generate a 'Closure (Process ())'
-- value and a function to add the closure to a remote table.
mkProcBaseClosure
  :: [Char]
  -> Process ()
  -> (Closure (Process ()), RemoteTable -> RemoteTable)
mkProcBaseClosure name p = (procClosure, registerProc p)
  where
    procClosure = staticClosure (staticLabel name)
    registerProc = registerStatic name . toDynamic

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Find a service on the local node within the given timeout
findLocalService
  :: (MonadProcessBase m, Service s)
  => s -> Int -> m (Maybe ProcessId)
findLocalService s timeout =
  findLocalProc (show s) timeout

findLocalProc
  :: (MonadProcessBase m)
  => [Char] -> Int -> m (Maybe ProcessId)
findLocalProc name timeout = do
  whereis name >>= \case
    Nothing -> do
      let timeleft = timeout - 10000
      if timeleft <= 0
         then pure Nothing
         else do
           liftIO (threadDelay 10000)
           findLocalProc name timeout
    Just pid -> pure $ Just pid

-- | Waits for local service to spawn for the given time before spawning the
-- process passed as an argument.
waitLocalService
  :: (MonadProcessBase m, Service s)
  => s -> Int -> m a -> m (Maybe a)
waitLocalService s timeout proc = do
  waitLocalProc (show s) timeout proc

-- | Waits for local service to spawn for the given time before spawning the
-- process passed as an argument.
--
-- Warning: Terminates the caller process if the local service is not found
-- within the timeout.
waitLocalService'
  :: (MonadProcessBase m, Service s)
  => s -> Int -> m a -> m a
waitLocalService' s timeout proc = do
  mPid <- findLocalService s timeout
  case mPid of
    Nothing -> terminate
    Just _  -> proc

-- | Waits for local process to be registered with the given name.
waitLocalProc
  :: (MonadProcessBase m)
  => [Char] -> Int -> m a -> m (Maybe a)
waitLocalProc name timeout proc = do
  mPid <- findLocalProc name timeout
  case mPid of
    Nothing -> pure Nothing
    Just _  -> Just <$> proc

-- | Waits for local process to be registered with the given name.
--
-- Warning: Terminates the caller process if the local service is not found
waitLocalProc'
  :: (MonadProcessBase m)
  => [Char] -> Int -> m a -> m a
waitLocalProc' name timeout proc = do
  mPid <- waitLocalProc name timeout proc
  case mPid of
    Nothing -> terminate
    Just x  -> pure x

--------------------------------------------------------------------------------

-- | Find a service on the local node.
-- Warning: Blocking; Will continuously search for the service on the given node
-- until found. If the service doesn't exist, this function will never unblock.
findRemoteService
  :: (MonadProcessBase m, Service s)
  => NodeId -> s -> m (Maybe ProcessId)
findRemoteService nid s = do
  mRes <- findRemoteServiceTimeout nid s 100000
  case mRes of
    Nothing  -> findRemoteService nid s
    Just pid -> pure (Just pid)

-- | Find a service on the local node within the given timeout
findRemoteServiceTimeout
  :: (MonadProcessBase m, Service s)
  => NodeId -> s -> Int -> m (Maybe ProcessId)
findRemoteServiceTimeout nid s timeout = do
  whereisRemoteAsync nid (show s)
  reply <- expectTimeout timeout
  case reply of
    Just (WhereIsReply tasks (Just pid)) -> return (Just pid)
    _ -> do
      putText $ "Failed to connect to " <> show s <> " service at: " <> show nid
      pure Nothing
