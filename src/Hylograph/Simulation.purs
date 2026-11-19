-- | Hylograph Force Simulation
-- |
-- | High-level API for running force-directed simulations.
-- | Two engines with the same interface: the default, in PureScript
-- | (`hylograph-forces`, no D3), and the Rust/WASM kernel.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import Hylograph.Simulation (runSimulation, Engine(..), SimulationEvent(..))
-- | import Hylograph.Simulation.Emitter (subscribe)
-- |
-- | main = do
-- |   -- Run simulation - returns handle AND event emitter
-- |   { handle, events } <- runSimulation
-- |     { engine: Pure  -- or WASM (same config!)
-- |     , setup: setup "physics"
-- |         [ manyBody "charge" # withStrength (static (-30.0))
-- |         , center "center"
-- |         ]
-- |     , nodes: myNodes
-- |     , links: []
-- |     , container: "#my-svg"
-- |     , alphaMin: 0.001
-- |     }
-- |
-- |   -- Subscribe to events (framework-agnostic)
-- |   unsubscribe <- subscribe events \event -> case event of
-- |     Tick { alpha } -> log $ "Alpha: " <> show alpha
-- |     Completed -> log "Simulation converged!"
-- |     _ -> pure unit
-- |
-- |   -- Get nodes for rendering (use HATS or your own rendering)
-- |   nodes <- handle.getNodes
-- |
-- |   -- Update data (GUP semantics, auto-reheat)
-- |   result <- handle.updateData newNodes newLinks
-- | ```
-- |
-- | ## Framework Integration
-- |
-- | **Halogen**: Use `toHalogenEmitter` from `Hylograph.Simulation.Halogen`
-- | **React**: Use `subscribe` in a `useEffect` hook with cleanup
-- | **Vanilla**: Just call `subscribe` directly
module Hylograph.Simulation
  ( -- * Main Entry Point
    runSimulation
  , SimulationResult

    -- * Configuration
  , SimulationConfig
  , Engine(..)

    -- * Handle (for controlling the simulation)
  , SimulationHandle

    -- * Position interpolation types
  , PositionMap

    -- * Events (framework-agnostic)
  , module EmitterExports

    -- * WASM Initialization
  , initWASM

    -- * Re-exports for convenience
  , module SetupExports
  , module SimNodeExports
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref as Ref

-- Coordinator
import Hylograph.Transition.Coordinator as Coord

-- Emitter
import Hylograph.Simulation.Emitter as Emitter
-- For local use
import Hylograph.Simulation.Emitter (SimulationEmitter, SimulationEvent(..))
-- For re-export
import Hylograph.Simulation.Emitter (SimulationEmitter, SimulationEvent(..), subscribe) as EmitterExports

-- The default engine
import Hylograph.ForceEngine.Simulation as ForceSim
import Hylograph.ForceEngine.Setup as ForceSetup
import Hylograph.ForceEngine.Setup
  ( Setup
  , GUPResult
  , GUPLinkResult
  , setup
  , manyBody
  , collide
  , link
  , center
  , positionX
  , positionY
  , radial
  , withStrength
  , withRadius
  , withX
  , withY
  , withDistance
  , withTheta
  , withIterations
  , withFilter
  , withDistanceMin
  , withDistanceMax
  , static
  , dynamic
  ) as SetupExports

-- Import types we need locally (also re-exported via SetupExports)
import Hylograph.ForceEngine.Setup (GUPResult, GUPLinkResult)

-- WASM Force Engine
import Hylograph.ForceEngine.Setup.WASM as WASMSetup

-- Node type re-export
import Hylograph.ForceEngine.Simulation (SimulationNode) as SimNodeExports

-- =============================================================================
-- Types
-- =============================================================================

-- | Position map for interpolation - keyed by node ID (as string)
type PositionMap = ForceSim.PositionMap

-- | Which engine runs the forces.
-- |
-- | - `Pure`: the default. `hylograph-forces`: d3-force's forces bit for
-- |   bit and the Rust kernel's additions, in PureScript. Nothing to load,
-- |   no D3.
-- | - `WASM`: the Rust/WASM kernel, the fast path. Needs `initWASM` first.
-- | - `D3`: deprecated, and the same as `Pure`. It is kept so that code
-- |   written when the default engine drove d3-force's functions still
-- |   compiles; there is no D3 behind it any more.
data Engine = Pure | WASM | D3

-- | Configuration for running a simulation
-- |
-- | Note: There's no `onComplete` callback - use the `events` emitter instead.
-- | This keeps the API framework-agnostic.
-- |
-- | Rendering is handled externally via the Tick event and `handle.getNodes`.
-- | Use HATS or any other rendering approach to render node positions on each tick.
type SimulationConfig r =
  { engine :: Engine
  , setup :: ForceSetup.Setup (ForceSim.SimulationNode r)
  , nodes :: Array (ForceSim.SimulationNode r)
  , links :: Array { source :: Int, target :: Int }
  , container :: String  -- CSS selector for SVG container (used for data tracking)
  , alphaMin :: Number   -- Convergence threshold (default: 0.001)
  }

-- | Result returned by runSimulation
-- |
-- | - `handle`: Control the simulation (update data, stop, start, etc.)
-- | - `events`: Subscribe to simulation events (Tick, Completed, etc.)
type SimulationResult r =
  { handle :: SimulationHandle r
  , events :: SimulationEmitter
  }

-- | Handle for controlling the simulation
type SimulationHandle r =
  { -- | Update nodes and links with GUP semantics (auto-reheats)
    updateData
      :: Array (ForceSim.SimulationNode r)
      -> Array { source :: Int, target :: Int }
      -> Effect { nodes :: GUPResult (ForceSim.SimulationNode r), links :: GUPLinkResult () }

    -- | Update force configuration (auto-reheats)
  , updateSetup :: ForceSetup.Setup (ForceSim.SimulationNode r) -> Effect Unit

    -- | Get current nodes with positions
  , getNodes :: Effect (Array (ForceSim.SimulationNode r))

    -- | Get current alpha value
  , getAlpha :: Effect Number

    -- | Stop the simulation
  , stop :: Effect Unit

    -- | Start/resume the simulation
  , start :: Effect Unit

    -- | Reheat the simulation (reset alpha to 1.0)
  , reheat :: Effect Unit

    -- | Interpolate node positions between start and target positions.
    -- | Progress should be 0.0 to 1.0 (apply easing before calling).
    -- | Used for animated transitions like quadrant layout.
  , interpolatePositions :: ForceSim.PositionMap -> ForceSim.PositionMap -> Number -> Effect Unit

    -- | Pin all nodes at their current positions (fx = x, fy = y).
    -- | Use before starting a transition to freeze current state.
  , pinNodes :: Effect Unit

    -- | Unpin all nodes (fx = null, fy = null).
    -- | Use after transition to let forces take over again.
  , unpinNodes :: Effect Unit
  }

-- =============================================================================
-- WASM Initialization
-- =============================================================================

-- | Initialize the WASM physics engine.
-- | Call once at application startup before using Engine.WASM.
-- |
-- | Example: `initWASM "./pkg/force_kernel.js"`
initWASM :: String -> Aff Unit
initWASM = WASMSetup.initWasm

-- =============================================================================
-- Main Entry Point
-- =============================================================================

-- | Run a force simulation with Hylograph visualization.
-- |
-- | Returns both a handle for controlling the simulation AND an event emitter
-- | for subscribing to simulation events. This design is framework-agnostic:
-- |
-- | - The emitter works with Halogen, React, or vanilla JS
-- | - Events include Tick, Started, Stopped, and Completed
-- | - The same code works for both engines
-- | Note: No Ord constraint required! We use renderTreeKeyed with a key function
-- | based on node.id, which doesn't require Ord on the datum type. This allows
-- | SimulationNode r (an extensible record) to work without needing Ord derivation.
runSimulation
  :: forall r
   . SimulationConfig r
  -> Effect (SimulationResult r)
runSimulation config = case config.engine of
  Pure -> runPureSimulation config
  D3 -> runPureSimulation config
  WASM -> runWASMSimulation config

-- =============================================================================
-- Default (pure PureScript) Engine Implementation
-- =============================================================================

runPureSimulation
  :: forall r
   . SimulationConfig r
  -> Effect (SimulationResult r)
runPureSimulation config = do
  -- Create emitter for events
  { emitter, handle: emitterHandle } <- Emitter.create

  -- Create simulation with alphaDecay from setup
  -- This is important because sim.config is immutable after creation
  let simConfig = ForceSim.defaultConfig
        { alphaDecay = config.setup.params.alphaDecay
        , alphaMin = config.alphaMin
        }
  sim <- ForceSim.create simConfig

  let
    -- The setup's params are the simulation's (as on the WASM engine),
    -- except that this call's own `alphaMin` decides when it has converged.
    keepAlphaMin = Ref.modify_ (_ { alphaMin = config.alphaMin }) sim.settings

  -- Apply initial setup with data
  _ <- ForceSetup.applySetupWithData config.setup config.nodes config.links sim
  keepAlphaMin

  -- Store current setup for updates
  setupRef <- Ref.new config.setup

  -- Store alpha for getAlpha
  alphaRef <- Ref.new 1.0

  -- Create coordinator
  coord <- Coord.create
  coordRef <- Ref.new coord

  -- Track if running
  runningRef <- Ref.new true

  -- Emit Started event
  Emitter.emit emitterHandle Started

  -- Register consumer
  _ <- Coord.register coord
    { tick: pureConsumer sim emitterHandle alphaRef config.alphaMin
    , onComplete: do
        Ref.write false runningRef
        Emitter.emit emitterHandle Completed
    }

  -- Start coordinator
  Coord.start coord

  let
    -- Heat the simulation up again and make sure something is ticking it.
    -- If it had run to completion the coordinator has stopped, so a new one
    -- is started: without that a reheat (a drag, say) would change nothing
    -- anyone could see.
    rekindle = do
      Ref.write 1.0 sim.alpha
      Ref.write 1.0 alphaRef
      Emitter.emit emitterHandle Started
      running <- Ref.read runningRef
      unless running do
        Ref.write true runningRef
        -- Stop the old coordinator before creating a new one (two would
        -- tick the simulation twice a frame)
        oldCoord <- Ref.read coordRef
        Coord.stop oldCoord
        c <- Coord.create
        Ref.write c coordRef
        _ <- Coord.register c
          { tick: pureConsumer sim emitterHandle alphaRef config.alphaMin
          , onComplete: do
              Ref.write false runningRef
              Emitter.emit emitterHandle Completed
          }
        Coord.start c

  -- Return handle and emitter
  pure
    { events: emitter
    , handle:
        { updateData: \nodes links -> do
            currentSetup <- Ref.read setupRef
            result <- ForceSetup.applySetupWithData currentSetup nodes links sim
            keepAlphaMin
            rekindle
            pure result
        , updateSetup: \newSetup -> do
            Ref.write newSetup setupRef
            ForceSetup.applySetup newSetup sim
            keepAlphaMin
            rekindle
        , getNodes: ForceSim.getNodes sim
        , getAlpha: Ref.read alphaRef
        , stop: do
            running <- Ref.read runningRef
            when running do
              Ref.write false runningRef
              oldCoord <- Ref.read coordRef
              Coord.stop oldCoord
              Emitter.emit emitterHandle Stopped
        , start: do
            running <- Ref.read runningRef
            unless running rekindle
        , reheat: rekindle
        , interpolatePositions: \startPos targetPos progress ->
            ForceSim.interpolatePositionsInPlace startPos targetPos progress sim
        , pinNodes: ForceSim.pinNodesInPlace sim
        , unpinNodes: ForceSim.unpinNodesInPlace sim
        }
    }

-- | The default engine's consumer for the coordinator - emits Tick events
-- | Rendering is handled by callers subscribing to Tick events
pureConsumer
  :: forall r
   . ForceSim.Simulation r ()
  -> Emitter.EmitterHandle
  -> Ref.Ref Number
  -> Number
  -> Coord.Milliseconds
  -> Effect Coord.TickResult
pureConsumer sim emitterHandle alphaRef alphaMin _deltaMs = do
  -- Advance the simulation by one tick
  alpha <- ForceSim.tick sim
  Ref.write alpha alphaRef
  nodes <- ForceSim.getNodes sim
  Emitter.emit emitterHandle (Tick { alpha, nodeCount: Array.length nodes })
  pure $ if alpha < alphaMin
    then Coord.Completed
    else Coord.Converged alpha

-- =============================================================================
-- WASM Engine Implementation
-- =============================================================================

runWASMSimulation
  :: forall r
   . SimulationConfig r
  -> Effect (SimulationResult r)
runWASMSimulation config = do
  -- Create emitter for events
  { emitter, handle: emitterHandle } <- Emitter.create

  -- Create WASM simulation with initial data
  sim <- WASMSetup.create config.nodes config.links

  -- Apply initial setup
  _ <- WASMSetup.applySetupWithData config.setup config.nodes config.links sim

  -- Store current setup for updates
  setupRef <- Ref.new config.setup

  -- Store alpha for getAlpha
  alphaRef <- Ref.new 1.0

  -- Create coordinator
  coord <- Coord.create
  coordRef <- Ref.new coord

  -- Track if running
  runningRef <- Ref.new true

  -- Emit Started event
  Emitter.emit emitterHandle Started

  -- Register consumer
  _ <- Coord.register coord
    { tick: wasmConsumer sim emitterHandle alphaRef config.alphaMin
    , onComplete: do
        Ref.write false runningRef
        Emitter.emit emitterHandle Completed
    }

  -- Start coordinator
  Coord.start coord

  let
    -- As on the default engine: heat the simulation up again and make sure
    -- something is ticking it. If it had run to completion the coordinator
    -- has stopped, so a new one is started; without that a reheat (a drag,
    -- say) would change nothing anyone could see.
    rekindle = do
      WASMSetup.reheat sim
      Ref.write 1.0 alphaRef
      Emitter.emit emitterHandle Started
      running <- Ref.read runningRef
      unless running do
        Ref.write true runningRef
        oldCoord <- Ref.read coordRef
        Coord.stop oldCoord
        c <- Coord.create
        Ref.write c coordRef
        _ <- Coord.register c
          { tick: wasmConsumer sim emitterHandle alphaRef config.alphaMin
          , onComplete: do
              Ref.write false runningRef
              Emitter.emit emitterHandle Completed
          }
        Coord.start c

  -- Return handle and emitter
  pure
    { events: emitter
    , handle:
        { updateData: \nodes links -> do
            currentSetup <- Ref.read setupRef
            result <- WASMSetup.applySetupWithData currentSetup nodes links sim
            rekindle
            pure result
        , updateSetup: \newSetup -> do
            Ref.write newSetup setupRef
            WASMSetup.applySetup newSetup sim
            rekindle
        , getNodes: WASMSetup.getNodes sim
        , getAlpha: Ref.read alphaRef
        , stop: do
            running <- Ref.read runningRef
            when running do
              Ref.write false runningRef
              oldCoord <- Ref.read coordRef
              Coord.stop oldCoord
              Emitter.emit emitterHandle Stopped
        , start: do
            running <- Ref.read runningRef
            unless running rekindle
        , reheat: rekindle
        , interpolatePositions: \startPos targetPos progress ->
            WASMSetup.interpolatePositions startPos targetPos progress sim
        , pinNodes: WASMSetup.pinNodes sim
        , unpinNodes: WASMSetup.unpinNodes sim
        }
    }

-- | WASM consumer for coordinator - emits Tick events
-- | Rendering is handled by callers subscribing to Tick events
wasmConsumer
  :: forall r
   . WASMSetup.WASMSim r
  -> Emitter.EmitterHandle
  -> Ref.Ref Number
  -> Number
  -> Coord.Milliseconds
  -> Effect Coord.TickResult
wasmConsumer sim emitterHandle alphaRef alphaMin _deltaMs = do
  alpha <- WASMSetup.tick sim
  Ref.write alpha alphaRef
  nodes <- WASMSetup.getNodes sim
  Emitter.emit emitterHandle (Tick { alpha, nodeCount: Array.length nodes })
  pure $ if alpha < alphaMin
    then Coord.Completed
    else Coord.Converged alpha
