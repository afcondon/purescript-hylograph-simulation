-- | PSD3 Force Simulation
-- |
-- | High-level API for running force-directed simulations with PSD3 visualization.
-- | Supports both D3.js and WASM physics engines with the same interface.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import PSD3.Simulation (runSimulation, Engine(..), SimulationEvent(..))
-- | import PSD3.Simulation.Emitter (subscribe)
-- |
-- | main = do
-- |   -- Run simulation - returns handle AND event emitter
-- |   { handle, events } <- runSimulation
-- |     { engine: D3  -- or WASM (same config!)
-- |     , setup: setup "physics"
-- |         [ manyBody "charge" # withStrength (static (-30.0))
-- |         , center "center"
-- |         ]
-- |     , nodes: myNodes
-- |     , links: []
-- |     , container: "#my-svg"
-- |     , nodeTemplate: \_ -> A.elem Circle [...]
-- |     , alphaMin: 0.001
-- |     }
-- |
-- |   -- Subscribe to events (framework-agnostic)
-- |   unsubscribe <- subscribe events \event -> case event of
-- |     Tick { alpha } -> log $ "Alpha: " <> show alpha
-- |     Completed -> log "Simulation converged!"
-- |     _ -> pure unit
-- |
-- |   -- Update data (GUP semantics, auto-reheat)
-- |   result <- handle.updateData newNodes newLinks
-- | ```
-- |
-- | ## Framework Integration
-- |
-- | **Halogen**: Use `toHalogenEmitter` from `PSD3.Simulation.Halogen`
-- | **React**: Use `subscribe` in a `useEffect` hook with cleanup
-- | **Vanilla**: Just call `subscribe` directly
module PSD3.Simulation
  ( -- * Main Entry Point
    runSimulation
  , SimulationResult

    -- * Configuration
  , SimulationConfig
  , Engine(..)

    -- * Handle (for controlling the simulation)
  , SimulationHandle

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

-- PSD3 Visualization
import PSD3.AST as A
import PSD3.Render (runD3, select, renderTree)

-- Coordinator
import PSD3.Transition.Coordinator as Coord

-- Emitter
import PSD3.Simulation.Emitter as Emitter
-- For local use
import PSD3.Simulation.Emitter (SimulationEmitter, SimulationEvent(..))
-- For re-export
import PSD3.Simulation.Emitter (SimulationEmitter, SimulationEvent(..), subscribe) as EmitterExports

-- D3 Force Engine
import PSD3.ForceEngine.Simulation as D3Sim
import PSD3.ForceEngine.Setup as D3Setup
import PSD3.ForceEngine.Setup
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
import PSD3.ForceEngine.Setup (GUPResult, GUPLinkResult)

-- WASM Force Engine
import PSD3.ForceEngine.Setup.WASM as WASMSetup

-- Node type re-export
import PSD3.ForceEngine.Simulation (SimulationNode) as SimNodeExports

-- =============================================================================
-- Types
-- =============================================================================

-- | Physics engine selection
data Engine = D3 | WASM

-- | Configuration for running a simulation
-- |
-- | Note: There's no `onComplete` callback - use the `events` emitter instead.
-- | This keeps the API framework-agnostic.
type SimulationConfig r =
  { engine :: Engine
  , setup :: D3Setup.Setup (D3Sim.SimulationNode r)
  , nodes :: Array (D3Sim.SimulationNode r)
  , links :: Array { source :: Int, target :: Int }
  , container :: String  -- CSS selector for SVG container
  , nodeTemplate :: D3Sim.SimulationNode r -> A.Tree (D3Sim.SimulationNode r)
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
      :: Array (D3Sim.SimulationNode r)
      -> Array { source :: Int, target :: Int }
      -> Effect { nodes :: GUPResult (D3Sim.SimulationNode r), links :: GUPLinkResult () }

    -- | Update force configuration (auto-reheats)
  , updateSetup :: D3Setup.Setup (D3Sim.SimulationNode r) -> Effect Unit

    -- | Get current nodes with positions
  , getNodes :: Effect (Array (D3Sim.SimulationNode r))

    -- | Get current alpha value
  , getAlpha :: Effect Number

    -- | Stop the simulation
  , stop :: Effect Unit

    -- | Start/resume the simulation
  , start :: Effect Unit

    -- | Reheat the simulation (reset alpha to 1.0)
  , reheat :: Effect Unit
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

-- | Run a force simulation with PSD3 visualization.
-- |
-- | Returns both a handle for controlling the simulation AND an event emitter
-- | for subscribing to simulation events. This design is framework-agnostic:
-- |
-- | - The emitter works with Halogen, React, or vanilla JS
-- | - Events include Tick, Started, Stopped, and Completed
-- | - The same code works for both D3 and WASM engines
-- |
-- | Note: The node type must have an Ord instance for data join rendering.
runSimulation
  :: forall r
   . Ord (D3Sim.SimulationNode r)
  => SimulationConfig r
  -> Effect (SimulationResult r)
runSimulation config = case config.engine of
  D3 -> runD3Simulation config
  WASM -> runWASMSimulation config

-- =============================================================================
-- D3 Engine Implementation
-- =============================================================================

runD3Simulation
  :: forall r
   . Ord (D3Sim.SimulationNode r)
  => SimulationConfig r
  -> Effect (SimulationResult r)
runD3Simulation config = do
  -- Create emitter for events
  { emitter, handle: emitterHandle } <- Emitter.create

  -- Create simulation
  sim <- D3Sim.create D3Sim.defaultConfig

  -- Apply initial setup with data
  _ <- D3Setup.applySetupWithData config.setup config.nodes config.links sim

  -- Store current setup for updates
  setupRef <- Ref.new config.setup

  -- Store alpha for getAlpha
  alphaRef <- Ref.new 1.0

  -- Create coordinator
  coord <- Coord.create
  coordRef <- Ref.new coord

  -- Initial render
  renderNodes config sim

  -- Track if running
  runningRef <- Ref.new true

  -- Emit Started event
  Emitter.emit emitterHandle Started

  -- Register consumer
  _ <- Coord.register coord
    { tick: d3Consumer sim emitterHandle alphaRef config.alphaMin do
        renderNodes config sim
    , onComplete: do
        Ref.write false runningRef
        Emitter.emit emitterHandle Completed
    }

  -- Start coordinator
  Coord.start coord

  -- Return handle and emitter
  pure
    { events: emitter
    , handle:
        { updateData: \nodes links -> do
            currentSetup <- Ref.read setupRef
            result <- D3Setup.applySetupWithData currentSetup nodes links sim
            D3Sim.reheat sim
            Ref.write 1.0 alphaRef
            Emitter.emit emitterHandle Started
            renderNodes config sim
            pure result

        , updateSetup: \newSetup -> do
            Ref.write newSetup setupRef
            D3Setup.applySetup newSetup sim
            D3Sim.reheat sim
            Ref.write 1.0 alphaRef
            Emitter.emit emitterHandle Started

        , getNodes: D3Sim.getNodes sim

        , getAlpha: Ref.read alphaRef

        , stop: do
            running <- Ref.read runningRef
            when running do
              Ref.write false runningRef
              Emitter.emit emitterHandle Stopped

        , start: do
            running <- Ref.read runningRef
            unless running do
              D3Sim.reheat sim
              Ref.write 1.0 alphaRef
              Ref.write true runningRef
              Emitter.emit emitterHandle Started
              -- Re-register and restart
              c <- Coord.create
              Ref.write c coordRef
              _ <- Coord.register c
                { tick: d3Consumer sim emitterHandle alphaRef config.alphaMin do
                    renderNodes config sim
                , onComplete: do
                    Ref.write false runningRef
                    Emitter.emit emitterHandle Completed
                }
              Coord.start c

        , reheat: do
            D3Sim.reheat sim
            Ref.write 1.0 alphaRef
            Emitter.emit emitterHandle Started
        }
    }

-- | D3 consumer for coordinator - emits Tick events
d3Consumer
  :: forall r
   . D3Sim.Simulation r ()
  -> Emitter.EmitterHandle
  -> Ref.Ref Number
  -> Number
  -> Effect Unit
  -> Coord.Milliseconds
  -> Effect Coord.TickResult
d3Consumer sim emitterHandle alphaRef alphaMin onTick _deltaMs = do
  -- Advance the D3 force simulation by one tick
  alpha <- D3Sim.tick sim
  Ref.write alpha alphaRef
  nodes <- D3Sim.getNodes sim
  Emitter.emit emitterHandle (Tick { alpha, nodeCount: Array.length nodes })
  onTick
  pure $ if alpha < alphaMin
    then Coord.Completed
    else Coord.Converged alpha

-- | Render nodes using PSD3
renderNodes
  :: forall r
   . Ord (D3Sim.SimulationNode r)
  => SimulationConfig r
  -> D3Sim.Simulation r ()
  -> Effect Unit
renderNodes config sim = do
  nodes <- D3Sim.getNodes sim
  void $ runD3 do
    container <- select config.container
    renderTree container (A.joinData "sim-nodes" "circle" nodes config.nodeTemplate)

-- =============================================================================
-- WASM Engine Implementation
-- =============================================================================

runWASMSimulation
  :: forall r
   . Ord (D3Sim.SimulationNode r)
  => SimulationConfig r
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

  -- Initial render
  renderWASMNodes config sim

  -- Track if running
  runningRef <- Ref.new true

  -- Emit Started event
  Emitter.emit emitterHandle Started

  -- Register consumer
  _ <- Coord.register coord
    { tick: wasmConsumer sim emitterHandle alphaRef config.alphaMin do
        renderWASMNodes config sim
    , onComplete: do
        Ref.write false runningRef
        Emitter.emit emitterHandle Completed
    }

  -- Start coordinator
  Coord.start coord

  -- Return handle and emitter
  pure
    { events: emitter
    , handle:
        { updateData: \nodes links -> do
            currentSetup <- Ref.read setupRef
            result <- WASMSetup.applySetupWithData currentSetup nodes links sim
            WASMSetup.reheat sim
            Ref.write 1.0 alphaRef
            Emitter.emit emitterHandle Started
            renderWASMNodes config sim
            pure result

        , updateSetup: \newSetup -> do
            Ref.write newSetup setupRef
            WASMSetup.applySetup newSetup sim
            WASMSetup.reheat sim
            Ref.write 1.0 alphaRef
            Emitter.emit emitterHandle Started

        , getNodes: WASMSetup.getNodes sim

        , getAlpha: Ref.read alphaRef

        , stop: do
            running <- Ref.read runningRef
            when running do
              Ref.write false runningRef
              Emitter.emit emitterHandle Stopped

        , start: do
            running <- Ref.read runningRef
            unless running do
              WASMSetup.reheat sim
              Ref.write 1.0 alphaRef
              Ref.write true runningRef
              Emitter.emit emitterHandle Started
              c <- Coord.create
              Ref.write c coordRef
              _ <- Coord.register c
                { tick: wasmConsumer sim emitterHandle alphaRef config.alphaMin do
                    renderWASMNodes config sim
                , onComplete: do
                    Ref.write false runningRef
                    Emitter.emit emitterHandle Completed
                }
              Coord.start c

        , reheat: do
            WASMSetup.reheat sim
            Ref.write 1.0 alphaRef
            Emitter.emit emitterHandle Started
        }
    }

-- | Render WASM nodes using PSD3
renderWASMNodes
  :: forall r
   . Ord (D3Sim.SimulationNode r)
  => SimulationConfig r
  -> WASMSetup.WASMSim r
  -> Effect Unit
renderWASMNodes config sim = do
  nodes <- WASMSetup.getNodes sim
  void $ runD3 do
    container <- select config.container
    renderTree container (A.joinData "sim-nodes" "circle" nodes config.nodeTemplate)

-- | WASM consumer for coordinator - emits Tick events
wasmConsumer
  :: forall r
   . WASMSetup.WASMSim r
  -> Emitter.EmitterHandle
  -> Ref.Ref Number
  -> Number
  -> Effect Unit
  -> Coord.Milliseconds
  -> Effect Coord.TickResult
wasmConsumer sim emitterHandle alphaRef alphaMin onTick _deltaMs = do
  alpha <- WASMSetup.tick sim
  Ref.write alpha alphaRef
  nodes <- WASMSetup.getNodes sim
  Emitter.emit emitterHandle (Tick { alpha, nodeCount: Array.length nodes })
  onTick
  pure $ if alpha < alphaMin
    then Coord.Completed
    else Coord.Converged alpha
