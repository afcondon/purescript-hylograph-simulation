-- | Tick Consumers
-- |
-- | Adapters that wrap various animation sources as TickConsumers
-- | for use with the Coordinator.
-- |
-- | Currently supports:
-- | - TransitionState from Engine.purs
-- | - TransitionGroup from Engine.purs
-- |
-- | Future:
-- | - D3 force simulation
-- | - WASM force kernel
module PSD3.Transition.Consumers
  ( -- * Transition consumers
    transitionConsumer
  , transitionGroupConsumer
    -- * Re-exports for convenience
  , module Engine
  , module Coordinator
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.Transition.Coordinator (TickResult(..), Milliseconds)
import PSD3.Transition.Coordinator (TickResult(..), Milliseconds) as Coordinator
import PSD3.Transition.Engine (TransitionState, TransitionGroup)
import PSD3.Transition.Engine (start, tick, currentValue, isComplete, group, groupTick, groupValues, groupComplete) as Engine

-- =============================================================================
-- Single Transition Consumer
-- =============================================================================

-- | Create a tick function for a single transition.
-- |
-- | Takes a Ref to the transition state and an effect to run on each tick
-- | with the current value. Returns Completed when the transition finishes.
-- |
-- | Usage:
-- | ```purescript
-- | stateRef <- Ref.new (Engine.start mySpec)
-- | let consumer = transitionConsumer stateRef \value -> setOpacity element value
-- | C.register coord { tick: consumer, onComplete: log "done" }
-- | ```
transitionConsumer
  :: forall a
   . Ref (TransitionState a)
  -> (a -> Effect Unit)          -- ^ Effect to run with current value
  -> Milliseconds
  -> Effect TickResult
transitionConsumer stateRef onValue deltaMs = do
  state <- Ref.read stateRef
  let newState = Engine.tick deltaMs state
  Ref.write newState stateRef
  onValue (Engine.currentValue newState)
  pure $ if Engine.isComplete newState
    then Completed
    else StillRunning

-- =============================================================================
-- Transition Group Consumer
-- =============================================================================

-- | Create a tick function for a group of coordinated transitions.
-- |
-- | Takes a Ref to the group and an effect to run on each tick with
-- | all current values. Returns Completed when all transitions finish.
-- |
-- | Usage:
-- | ```purescript
-- | groupRef <- Ref.new (Engine.group [spec1, spec2, spec3])
-- | let consumer = transitionGroupConsumer groupRef \[x, y, opacity] -> do
-- |       setPosition element x y
-- |       setOpacity element opacity
-- | C.register coord { tick: consumer, onComplete: log "all done" }
-- | ```
transitionGroupConsumer
  :: Ref TransitionGroup
  -> (Array Number -> Effect Unit)  -- ^ Effect to run with all current values
  -> Milliseconds
  -> Effect TickResult
transitionGroupConsumer groupRef onValues deltaMs = do
  grp <- Ref.read groupRef
  let newGroup = Engine.groupTick deltaMs grp
  Ref.write newGroup groupRef
  onValues (Engine.groupValues newGroup)
  pure $ if Engine.groupComplete newGroup
    then Completed
    else StillRunning

-- =============================================================================
-- Future: Simulation Consumers
-- =============================================================================

-- TODO: D3 simulation consumer
-- d3SimulationConsumer :: D3Simulation -> (Array Position -> Effect Unit) -> Milliseconds -> Effect TickResult

-- TODO: WASM simulation consumer
-- wasmSimulationConsumer :: WASMSimulation -> (Array Position -> Effect Unit) -> Milliseconds -> Effect TickResult
