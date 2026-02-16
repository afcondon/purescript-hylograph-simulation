-- | PSD3 Visualization Test
-- |
-- | Demonstrates the TickCoordinator driving PSD3 visualization using:
-- | - applySetupWithData for declarative simulation management (GUP semantics)
-- | - HATS for declarative visualization specification
-- | - forEach for data-driven DOM updates
-- | - InterpreterTick for rendering
-- |
-- | This shows the full unified architecture:
-- | Coordinator (single RAF) → Consumers (tick) → applySetupWithData (GUP) → HATS Tree → DOM
-- |
-- | **Key feature**: Both D3 and WASM simulations use the SAME Setup API!
module Test.PSD3VisualTest where

import Prelude

import Data.Nullable (null) as Nullable
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

-- HATS Visualization
import Hylograph.HATS (Tree, forEach, elem, thunkedNum, staticStr)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- Coordinator and Consumers
import Hylograph.Transition.Coordinator (Coordinator, Milliseconds, TickResult(..), create, register, start) as C
import Hylograph.Transition.Consumers (simulationConsumer)

-- Force simulation with declarative setup (D3 engine)
import Hylograph.ForceEngine.Simulation as Sim
import Hylograph.ForceEngine.Setup as Setup
import Hylograph.ForceEngine.Setup (setup, manyBody, center, withStrength, withX, withY, static)

-- WASM simulation with SAME Setup API (functional parity!)
import Hylograph.ForceEngine.Setup.WASM as WASMSetup

-- =============================================================================
-- Shared Types
-- =============================================================================

-- | Node type - SAME for both D3 and WASM simulations!
-- | This is the key to functional parity.
type SimNode = Sim.SimulationNode (label :: String)

-- =============================================================================
-- Main Entry Point
-- =============================================================================

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "PSD3 Visualization Test Starting..."
  liftEffect $ log "Demonstrating UNIFIED Setup API for D3 and WASM"

  -- Initialize WASM (using unified Setup module)
  liftEffect $ log "Initializing WASM force kernel..."
  WASMSetup.initWasm "./wasm-pkg/force_kernel.js"
  liftEffect $ log "WASM initialized!"

  liftEffect do
    -- Create coordinator
    coord <- C.create

    -- Setup D3 simulation with PSD3 visualization
    setupD3SimulationPSD3 coord

    -- Setup WASM simulation with SAME API
    setupWASMSimulationPSD3 coord

    -- Start the coordinator
    log "Starting coordinator..."
    C.start coord

    log "Both simulations running with unified Setup API!"

-- =============================================================================
-- Shared Force Configuration (works for BOTH D3 and WASM!)
-- =============================================================================

-- | Declarative force configuration - IDENTICAL for both engines
-- | This demonstrates DRY: one config, two engines
forceSetup :: forall node. Setup.Setup node
forceSetup = setup "physics"
  [ manyBody "charge" # withStrength (static (-80.0))
  , center "center" # withX (static 200.0) # withY (static 75.0) # withStrength (static 1.0)
  ]

-- | D3 configuration (slightly offset center)
d3ForceSetup :: Setup.Setup SimNode
d3ForceSetup = setup "physics"
  [ manyBody "charge" # withStrength (static (-80.0))
  , center "center" # withX (static 150.0) # withY (static 75.0) # withStrength (static 1.0)
  ]

-- | WASM configuration (offset to right side)
wasmForceSetup :: Setup.Setup SimNode
wasmForceSetup = setup "physics"
  [ manyBody "charge" # withStrength (static (-80.0))
  , center "center" # withX (static 350.0) # withY (static 75.0) # withStrength (static 1.0)
  ]

-- =============================================================================
-- D3 Force Simulation with Declarative Setup
-- =============================================================================

-- | Initial nodes for D3 simulation
initialD3Nodes :: Array SimNode
initialD3Nodes =
  [ mkNode 0 150.0 75.0 "A"
  , mkNode 1 160.0 80.0 "B"
  , mkNode 2 140.0 70.0 "C"
  , mkNode 3 155.0 85.0 "D"
  , mkNode 4 145.0 65.0 "E"
  , mkNode 5 165.0 75.0 "F"
  ]

setupD3SimulationPSD3 :: C.Coordinator -> Effect Unit
setupD3SimulationPSD3 coord = do
  log "Setting up D3 force simulation with declarative applySetupWithData"

  -- Create simulation (empty - data comes from applySetupWithData)
  sim <- Sim.create Sim.defaultConfig

  -- Apply setup with initial data - this handles EVERYTHING:
  -- - Sets nodes (with position/velocity state management)
  -- - Configures forces (idempotent add/remove/update)
  -- - Returns enter/update/exit arrays for GUP animations
  result <- Setup.applySetupWithData d3ForceSetup initialD3Nodes [] sim

  log $ "  D3 Entered: " <> show (map _.label result.nodes.entered)

  -- Initial render
  void $ rerender "#d3-sim-psd3" (simulationTree "#06b6d4" initialD3Nodes)

  -- Register consumer - just ticks and renders, no state management needed
  _ <- C.register coord
    { tick: simulationConsumer sim 0.001 do
        -- Get nodes (simulation has updated x/y/vx/vy internally)
        currentNodes <- Sim.getNodes sim
        -- Render via HATS
        void $ rerender "#d3-sim-psd3" (simulationTree "#06b6d4" currentNodes)
    , onComplete: log "✓ D3 simulation (HATS) converged!"
    }
  pure unit

-- =============================================================================
-- WASM Force Simulation with SAME Setup API
-- =============================================================================

-- | Initial nodes for WASM simulation (SAME type as D3!)
initialWASMNodes :: Array SimNode
initialWASMNodes =
  [ mkNode 0 350.0 75.0 "1"
  , mkNode 1 360.0 80.0 "2"
  , mkNode 2 340.0 70.0 "3"
  , mkNode 3 355.0 85.0 "4"
  , mkNode 4 345.0 65.0 "5"
  , mkNode 5 365.0 75.0 "6"
  ]

setupWASMSimulationPSD3 :: C.Coordinator -> Effect Unit
setupWASMSimulationPSD3 coord = do
  log "Setting up WASM force simulation with SAME declarative API"

  -- Create WASM simulation with initial data
  sim <- WASMSetup.create initialWASMNodes []

  -- Apply setup - SAME function signature as D3!
  result <- WASMSetup.applySetupWithData wasmForceSetup initialWASMNodes [] sim

  log $ "  WASM Entered: " <> show (map _.label result.nodes.entered)

  -- Initial render
  void $ rerender "#wasm-sim-psd3" (simulationTree "#f97316" initialWASMNodes)

  -- Register consumer
  _ <- C.register coord
    { tick: wasmConsumer sim 0.001 do
        -- Get nodes - SAME interface as D3!
        currentNodes <- WASMSetup.getNodes sim
        -- Render via HATS
        void $ rerender "#wasm-sim-psd3" (simulationTree "#f97316" currentNodes)
    , onComplete: log "✓ WASM simulation (HATS) converged!"
    }
  pure unit

-- =============================================================================
-- Shared Visualization (works for BOTH engines!)
-- =============================================================================

-- | HATS tree for simulation visualization - works for ANY SimulationNode
-- | Pure function: nodes → visualization spec
simulationTree :: forall r. String -> Array (Sim.SimulationNode r) -> Tree
simulationTree color nodes =
  forEach "sim-nodes" Circle nodes (\n -> show n.id) \node ->
    elem Circle
      [ thunkedNum "cx" node.x
      , thunkedNum "cy" node.y
      , staticStr "r" "12"
      , staticStr "fill" color
      ]
      []

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Create a simulation node
mkNode :: Int -> Number -> Number -> String -> SimNode
mkNode id x y label =
  { id, x, y, vx: 0.0, vy: 0.0, fx: Nullable.null, fy: Nullable.null, label }

-- | WASM consumer - same signature as D3's simulationConsumer
wasmConsumer
  :: forall r
   . WASMSetup.WASMSim r
  -> Number
  -> Effect Unit
  -> C.Milliseconds
  -> Effect C.TickResult
wasmConsumer sim alphaMin onTick _deltaMs = do
  alpha <- WASMSetup.tick sim
  onTick
  pure $ if alpha < alphaMin
    then C.Completed
    else C.Converged alpha
