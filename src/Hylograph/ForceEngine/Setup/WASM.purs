-- | WASM Force Setup
-- |
-- | Declarative force configuration for WASM simulation engine.
-- | Uses the same Setup types as the default engine, and means the same.
-- |
-- | What a `Setup` means here, force by force, is what it means to the
-- | default engine (`Hylograph.ForceEngine.Setup.applySetup`), and
-- | `test/engine-parity.mjs` holds the two to each other:
-- | - any number of forces of any type; a later force replaces an earlier
-- |   one of the same name, and each tick applies them in name order (the
-- |   default engine keeps them in a `Map` keyed by name);
-- | - links name their endpoints by node `id`, not by array position;
-- | - new nodes are initialized as d3 initializes them
-- |   (`Hylograph.ForceEngine.Core.initializeNodes`).
-- |
-- | `Dynamic` values, filters, groups and regions are evaluated once per
-- | node (or per link) when the setup is applied and handed to the kernel
-- | as arrays; the kernel never calls back into JavaScript.
-- |
-- | Both engines are deterministic: the same nodes, links and setup give the
-- | same positions on every run. The kernel computes in f32 and the default
-- | engine in f64, so the two agree to float width over one tick and drift
-- | apart after that, as any two roundings of a force layout do.
-- |
-- | The nodes a caller is handed (`getNodes`, `step`) are fresh records
-- | after every tick, not the same objects moved, which is the one way this
-- | engine differs from the default one. A pin set on the latest ones
-- | (`node.fx`, as a drag does) is read before the next tick.
-- |
-- | Example:
-- | ```purescript
-- | import Hylograph.ForceEngine.Setup (setup, manyBody, center, withStrength, static)
-- | import Hylograph.ForceEngine.Setup.WASM as WASM
-- |
-- | mySetup = setup "physics"
-- |   [ manyBody "charge" # withStrength (static (-30.0))
-- |   , center "center" # withX (static 0.0) # withY (static 0.0)
-- |   ]
-- |
-- | main = do
-- |   WASM.initWasm "./pkg/force_kernel.js"
-- |   sim <- WASM.create initialNodes initialLinks
-- |   result <- WASM.applySetupWithData mySetup newNodes newLinks sim
-- |   -- Use result.nodes.entered, result.nodes.exited for animations
-- | ```
module Hylograph.ForceEngine.Setup.WASM
  ( -- * Types
    WASMSim
  -- Note: SimulationNode is re-exported via 'module Exports' below

    -- * Initialization
  , initWasm
  , isWasmReady

    -- * Simulation Lifecycle
  , create
  , createEmpty
  , free

    -- * Apply Setup (the key functions - functional parity with D3)
  , applySetup
  , tryApplySetup
  , applySetupWithData

    -- * Morphing, snapshots, convergence
  , retarget
  , retargetTo
  , reradius
  , snapshot
  , restore
  , kineticEnergy
  , maxDisplacement
  , maxAcceleration
  , settled

    -- * Direct Operations
  , step
  , tick
  , tickN
  , getNodes
  , getAlpha
  , reheat
  , isRunning
  , pinNodes
  , unpinNodes
  , interpolatePositions

    -- * Re-exports from Setup (for convenience)
  , module Exports
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (nan)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Hylograph.ForceEngine.Core as Core
import Hylograph.ForceEngine.WASM as FFI
import Hylograph.ForceEngine.Simulation (SimulationNode) as Exports
import Hylograph.ForceEngine.Simulation (PositionMap, SimulationNode, interpolatePositionsInPlace_, pinNodesInPlace_, unpinNodesInPlace_)
import Data.Either (Either(..))
import Hylograph.ForceEngine.Setup
  ( Setup
  , SetupParams
  , ForceConfig
  , ForceType(..)
  , Value(..)
  , GUPResult
  , GUPLinkResult
  , NodeGUPInternal
  , computeNodeGUP
  , computeLinkGUP
  , valueToNumber
  , getForces
  , getParams
  ) as Exports
import Hylograph.ForceEngine.Setup
  ( Setup
  , ForceConfig(..)
  , LinkEnds
  , LinkStrength(..)
  , ManyBodyScope(..)
  , Region(..)
  , Value(..)
  , GUPResult
  , GUPLinkResult
  , computeNodeGUP
  , computeLinkGUP
  , forceName
  , linkProblem
  , linkStrengths
  , linkValues
  , resolveLinks
  , valueToNumber
  , getForces
  , getParams
  )

-- =============================================================================
-- Types
-- =============================================================================

-- | WASM Simulation handle.
-- | Wraps the low-level WASM handle with PureScript state management.
type WASMSim r =
  { wasmSim :: FFI.WASMSimulation
  , nodesRef :: Ref (Array (SimulationNode r))
  , linksRef :: Ref (Array { source :: Int, target :: Int })
  , forceIds :: Ref (Map String FFI.ForceId)  -- the kernel's id for each named force
  }

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the WASM module. Call once at application startup.
initWasm :: String -> Aff Unit
initWasm = FFI.initWasm

-- | Check if WASM is initialized
isWasmReady :: Effect Boolean
isWasmReady = FFI.isWasmReady

-- =============================================================================
-- Simulation Lifecycle
-- =============================================================================

-- | Create a WASM simulation with initial nodes and links.
create
  :: forall r
   . Array (SimulationNode r)
  -> Array { source :: Int, target :: Int }
  -> Effect (WASMSim r)
create nodes links = do
  wasmSim <- FFI.create (Array.length nodes)

  -- What the default engine does to new nodes, so that a Setup means the
  -- same on both: an index, a pinned node on its pin, a node with no
  -- position on d3's phyllotaxis spiral, a node with no velocity at rest.
  Core.initializeNodes nodes

  -- Sync initial positions, velocities and pinning to WASM
  FFI.syncNodesToWasm wasmSim nodes

  -- Set links
  setLinks wasmSim nodes links

  -- Create refs
  nodesRef <- Ref.new nodes
  linksRef <- Ref.new links
  forceIds <- Ref.new Map.empty

  pure { wasmSim, nodesRef, linksRef, forceIds }

-- | Create an empty WASM simulation (for use with applySetupWithData)
createEmpty :: forall r. Effect (WASMSim r)
createEmpty = do
  wasmSim <- FFI.create 0
  nodesRef <- Ref.new []
  linksRef <- Ref.new []
  forceIds <- Ref.new Map.empty
  pure { wasmSim, nodesRef, linksRef, forceIds }

-- | Free WASM simulation memory
free :: forall r. WASMSim r -> Effect Unit
free sim = FFI.free sim.wasmSim

-- =============================================================================
-- Apply Setup to WASM Simulation
-- =============================================================================

-- | Apply a Setup to a WASM simulation.
-- |
-- | Maps declarative force configuration to WASM kernel calls: the kernel's
-- | force list is emptied and refilled from the setup, so this is idempotent
-- | and can be called repeatedly with different setups. `Dynamic` values and
-- | filters are evaluated against the simulation's current nodes; apply the
-- | setup again after changing what they read.
-- |
-- | See the module header for how this compares with the default engine.
applySetup
  :: forall r
   . Setup (SimulationNode r)
  -> WASMSim r
  -> Effect Unit
applySetup setupConfig sim = do
  outcome <- tryApplySetup setupConfig sim
  case outcome of
    Left problem -> throw ("Hylograph.ForceEngine.Setup: " <> problem <> ". Nothing was applied.")
    Right _ -> pure unit

-- | `applySetup`, with the one thing that can go wrong as a value, as the
-- | default engine's `tryApplySetup` has it: `Left "node not found: <id>"`
-- | where the setup has a force along the links and a link names a node
-- | that is not among the nodes. The simulation is then left as it was.
tryApplySetup
  :: forall r
   . Setup (SimulationNode r)
  -> WASMSim r
  -> Effect (Either String Unit)
tryApplySetup setupConfig sim = do
  nodes <- Ref.read sim.nodesRef
  links <- Ref.read sim.linksRef
  case linkProblem setupConfig nodes links of
    Just problem -> pure (Left problem)
    Nothing -> Right <$> applyChecked setupConfig sim

applyChecked
  :: forall r
   . Setup (SimulationNode r)
  -> WASMSim r
  -> Effect Unit
applyChecked setupConfig sim = do
  nodes <- Ref.read sim.nodesRef
  links <- Ref.read sim.linksRef
  let ends = resolveLinks nodes links

  -- Every force goes in the kernel's force list; its six built-in forces
  -- (three of them on by default) are not used.
  FFI.enableForcesAll sim.wasmSim
    { manyBody: false, links: false, center: false, forceX: false, forceY: false, collide: false }
  FFI.clearForces sim.wasmSim
  ids <- traverse
    (\fc -> Tuple (forceName fc) <$> addForce sim.wasmSim nodes ends fc)
    (inApplicationOrder (getForces setupConfig))
  Ref.write (Map.fromFoldable ids) sim.forceIds

  -- Apply simulation params
  let params = getParams setupConfig
  FFI.setAlpha sim.wasmSim params.alpha
  FFI.setAlphaMin sim.wasmSim params.alphaMin
  FFI.setAlphaDecay sim.wasmSim params.alphaDecay
  FFI.setAlphaTarget sim.wasmSim params.alphaTarget
  FFI.setVelocityRetention sim.wasmSim params.velocityRetention

-- | The forces of a setup as the default engine applies them. It keeps forces
-- | in a `Map` keyed by name, so a later force replaces an earlier one of the
-- | same name and each tick runs them in name order, not in setup order.
inApplicationOrder :: forall node. Array (ForceConfig node) -> Array (ForceConfig node)
inApplicationOrder forces =
  Array.fromFoldable $ Map.values $ Map.fromFoldable $ map (\fc -> Tuple (forceName fc) fc) forces

-- | Add one force of a setup to the kernel's force list, evaluating its
-- | per-node and per-link values over the nodes and the links' ends.
addForce
  :: forall r
   . FFI.WASMSimulation
  -> Array (SimulationNode r)
  -> Array (LinkEnds (SimulationNode r))
  -> ForceConfig (SimulationNode r)
  -> Effect FFI.ForceId
addForce wasmSim nodes ends = case _ of
  ManyBodyForce c -> do
    force <- FFI.addManyBody wasmSim
      { strength: valueToNumber c.strength
      , theta: c.theta
      , distanceMin: c.distanceMin
      , distanceMax: c.distanceMax
      }
    perNode c.strength (FFI.setForceStrengths wasmSim force)
    filtered c.filter force
    case c.scope of
      AllPairs -> pure unit
      WithinGroups group -> do
        void $ FFI.setForceGroups wasmSim force (groupsOf group)
        void $ FFI.setManyBodyScope wasmSim force FFI.WithinGroups
      BetweenGroups group -> do
        void $ FFI.setForceGroups wasmSim force (groupsOf group)
        void $ FFI.setManyBodyScope wasmSim force FFI.BetweenGroups
    pure force

  CollideForce c -> do
    force <- FFI.addCollide wasmSim
      { radius: valueToNumber c.radius
      , strength: c.strength
      , iterations: c.iterations
      }
    perNode c.radius (FFI.setForceRadii wasmSim force)
    for_ c.filter \keep -> void $ FFI.setCollideFilter wasmSim force (map keep nodes)
    when c.alphaScaled $ void $ FFI.setCollideAlphaScaled wasmSim force true
    pure force

  LinkForce c -> do
    force <- FFI.addLinks wasmSim
      { distance: valueToNumber c.distance
      , strength: 1.0
      , iterations: c.iterations
      }
    perLink c.distance (FFI.setForceDistances wasmSim force)
    case c.strength of
      LinkStrength (Static strength) ->
        void $ FFI.setForceStrengths wasmSim force (map (const strength) ends)
      strength ->
        void $ FFI.setForceStrengths wasmSim force (linkStrengths strength ends)
    pure force

  CenterForce c ->
    FFI.addCenter wasmSim { x: c.x, y: c.y, strength: c.strength }

  PositionXForce c -> do
    force <- FFI.addForceX wasmSim
      { target: valueToNumber c.target
      , strength: valueToNumber c.strength
      }
    perNode c.target (FFI.setForceTargets wasmSim force)
    perNode c.strength (FFI.setForceStrengths wasmSim force)
    filtered c.filter force
    pure force

  PositionYForce c -> do
    force <- FFI.addForceY wasmSim
      { target: valueToNumber c.target
      , strength: valueToNumber c.strength
      }
    perNode c.target (FFI.setForceTargets wasmSim force)
    perNode c.strength (FFI.setForceStrengths wasmSim force)
    filtered c.filter force
    pure force

  RadialForce c -> do
    force <- FFI.addRadial wasmSim
      { radius: valueToNumber c.radius
      , x: c.x
      , y: c.y
      , strength: valueToNumber c.strength
      }
    perNode c.radius (FFI.setForceRadii wasmSim force)
    perNode c.strength (FFI.setForceStrengths wasmSim force)
    filtered c.filter force
    pure force

  TowardsForce c -> do
    force <- FFI.addTowards wasmSim (valueToNumber c.strength)
    void $ FFI.setForceTargetsXY wasmSim force (flatTargets c.target nodes)
    perNode c.strength (FFI.setForceStrengths wasmSim force)
    filtered c.filter force
    pure force

  RectCollideForce c -> do
    force <- FFI.addRectCollide wasmSim
      { halfWidth: valueToNumber c.halfWidth
      , halfHeight: valueToNumber c.halfHeight
      , strength: c.strength
      , iterations: c.iterations
      }
    perNode c.halfWidth (FFI.setForceHalfWidths wasmSim force)
    perNode c.halfHeight (FFI.setForceHalfHeights wasmSim force)
    for_ c.filter \keep -> void $ FFI.setCollideFilter wasmSim force (map keep nodes)
    case c.scope of
      AllPairs -> pure unit
      WithinGroups group -> do
        void $ FFI.setForceGroups wasmSim force (groupsOf group)
        void $ FFI.setRectCollidePairs wasmSim force FFI.WithinGroups
      BetweenGroups group -> do
        void $ FFI.setForceGroups wasmSim force (groupsOf group)
        void $ FFI.setRectCollidePairs wasmSim force FFI.BetweenGroups
    when c.throughSqueeze $ void $ FFI.setRectCollideThrough wasmSim force true
    pure force

  ContainForce c -> do
    force <- FFI.addContainment wasmSim { strength: c.strength, hard: c.hard }
    for_ c.regions case _ of
      RectRegion rect -> void $ FFI.containmentAddRect wasmSim force rect
      CircleRegion circle -> void $ FFI.containmentAddCircle wasmSim force circle
      PolygonRegion vertices -> void $ FFI.containmentAddPolygon wasmSim force vertices
    void $ FFI.setForceRegions wasmSim force (map c.region nodes)
    void $ FFI.setForceRadii wasmSim force (case c.radius of
      Static radius -> map (const radius) nodes
      Dynamic f -> map f nodes)
    void $ FFI.setForceHalfWidths wasmSim force (case c.halfWidth of
      Static extent -> map (const extent) nodes
      Dynamic f -> map f nodes)
    void $ FFI.setForceHalfHeights wasmSim force (case c.halfHeight of
      Static extent -> map (const extent) nodes
      Dynamic f -> map f nodes)
    pure force

  CohesionForce c -> do
    force <- FFI.addCohesion wasmSim (valueToNumber c.strength)
    void $ FFI.setForceGroups wasmSim force (groupsOf c.group)
    perNode c.strength (FFI.setForceStrengths wasmSim force)
    filtered c.filter force
    pure force

  DirectionalForce c -> do
    force <- FFI.addDirectional wasmSim
      { gap: valueToNumber c.gap
      , axis: c.axis
      , strength: valueToNumber c.strength
      , iterations: c.iterations
      }
    perLink c.gap (FFI.setForceGaps wasmSim force)
    perLink c.strength (FFI.setForceStrengths wasmSim force)
    when c.hard $ void $ FFI.setDirectionalHard wasmSim force true
    pure force
  where
  -- A Dynamic value becomes one number per node; a Static one is already
  -- in the force as added.
  perNode :: Value (SimulationNode r) Number -> (Array Number -> Effect Boolean) -> Effect Unit
  perNode value send = case value of
    Static _ -> pure unit
    Dynamic f -> void $ send (map f nodes)

  -- Likewise per link.
  perLink :: Value (LinkEnds (SimulationNode r)) Number -> (Array Number -> Effect Boolean) -> Effect Unit
  perLink value send = case value of
    Static _ -> pure unit
    Dynamic _ -> void $ send (linkValues value ends)

  -- A filter becomes one Boolean per node.
  filtered :: Maybe (SimulationNode r -> Boolean) -> FFI.ForceId -> Effect Unit
  filtered filter force = for_ filter \keep -> void $ FFI.setForceMask wasmSim force (map keep nodes)

  -- Groups can be any Int; the kernel gets them renumbered from 0 in order
  -- of first appearance, which keeps the same nodes together.
  groupsOf :: (SimulationNode r -> Maybe Int) -> Array (Maybe Int)
  groupsOf group =
    let
      given = map group nodes
      numbering = Map.fromFoldable $ Array.mapWithIndex (\index g -> Tuple g index) $ Array.nub $ Array.catMaybes given
    in
      map (_ >>= \g -> Map.lookup g numbering) given

-- | Targets as the kernel takes them: [x0, y0, x1, y1, ...], NaN for a node
-- | with none.
flatTargets :: forall node. (node -> Maybe { x :: Number, y :: Number }) -> Array node -> Array Number
flatTargets target = Array.concatMap \node -> case target node of
  Just point -> [ point.x, point.y ]
  Nothing -> [ nan, nan ]

-- | Hand the links to the kernel. Links name their endpoints by node `id`,
-- | as they do for the default engine; the kernel wants positions in the
-- | node array. A link naming a node that is not there cannot be given to
-- | the kernel and is left out here; `applySetup` is where that becomes an
-- | error, if the setup has a force that would have used it.
setLinks
  :: forall r
   . FFI.WASMSimulation
  -> Array (SimulationNode r)
  -> Array { source :: Int, target :: Int }
  -> Effect Unit
setLinks wasmSim nodes links = do
  let
    indexOfId = Map.fromFoldable $ Array.mapWithIndex (\index node -> Tuple node.id index) nodes
    endpoints link = ado
      source <- Map.lookup link.source indexOfId
      target <- Map.lookup link.target indexOfId
      in [ source, target ]
    resolved = Array.mapMaybe endpoints links
  FFI.setLinks wasmSim (Array.concat resolved)

-- =============================================================================
-- Apply Setup with Data (GUP semantics)
-- =============================================================================

-- | Apply setup with new nodes and links, using GUP semantics.
-- |
-- | This provides functional parity with the default engine's applySetupWithData:
-- | 1. Force configuration (via applySetup)
-- | 2. Node GUP: diff desired vs current, preserve simulation state
-- | 3. Link GUP: diff desired vs current
-- |
-- | Simulation state preserved for updated nodes:
-- | - x, y: Current position
-- | - vx, vy: Current velocity
-- | - fx, fy: Pinned position
-- |
-- | Example:
-- | ```purescript
-- | result <- applySetupWithData mySetup newNodes newLinks sim
-- | animateEnter result.nodes.entered
-- | animateExit result.nodes.exited
-- | reheat sim
-- | ```
applySetupWithData
  :: forall r
   . Setup (SimulationNode r)
  -> Array (SimulationNode r)
  -> Array { source :: Int, target :: Int }
  -> WASMSim r
  -> Effect { nodes :: GUPResult (SimulationNode r), links :: GUPLinkResult () }
applySetupWithData setupConfig desiredNodes desiredLinks sim = do
  -- Get current state
  currentNodes <- Ref.read sim.nodesRef
  currentLinks <- Ref.read sim.linksRef

  -- Compute node GUP (the default engine's logic)
  let nodeResult = computeNodeGUP currentNodes desiredNodes

  -- Write merged nodes to ref
  Ref.write nodeResult.merged sim.nodesRef

  -- Update WASM node count if changed
  let currentCount = Array.length currentNodes
  let newCount = Array.length nodeResult.merged
  when (currentCount /= newCount) do
    FFI.setNodeCount sim.wasmSim newCount

  -- As the default engine does: see `create`
  Core.initializeNodes nodeResult.merged

  -- Sync positions, velocities and pinning to WASM
  FFI.syncNodesToWasm sim.wasmSim nodeResult.merged

  -- Compute link GUP
  let linkResult = computeLinkGUP currentLinks (toSimpleLinks desiredLinks)

  -- Write links to ref and WASM
  Ref.write desiredLinks sim.linksRef
  setLinks sim.wasmSim nodeResult.merged desiredLinks

  -- Apply force configuration
  applySetup setupConfig sim

  -- Return GUP results for animation
  pure
    { nodes:
        { entered: nodeResult.entered
        , updated: nodeResult.updated
        , exited: nodeResult.exited
        }
    , links:
        { entered: linkResult.entered
        , updated: linkResult.updated
        , exited: linkResult.exited
        }
    }

-- | Convert links to simple source/target records for GUP computation
toSimpleLinks
  :: Array { source :: Int, target :: Int }
  -> Array { source :: Int, target :: Int }
toSimpleLinks = identity

-- =============================================================================
-- Morphing, snapshots, convergence
-- =============================================================================

-- | Give a Towards force new targets, by the name it has in the setup,
-- | without re-applying the setup. Cheap enough to call every tick: one
-- | array goes to the kernel and overwrites the old targets in place. For
-- | morphing between layouts. False if the setup has no Towards force of
-- | that name.
retarget
  :: forall r
   . String
  -> (SimulationNode r -> Maybe { x :: Number, y :: Number })
  -> WASMSim r
  -> Effect Boolean
retarget name target sim = do
  ids <- Ref.read sim.forceIds
  nodes <- Ref.read sim.nodesRef
  case Map.lookup name ids of
    Just force -> FFI.setForceTargetsXY sim.wasmSim force (flatTargets target nodes)
    Nothing -> pure false

-- | `retarget` with the points themselves, one for each node in the order
-- | of the simulation's nodes (`getNodes`). A node past the end of the array
-- | is left alone by the force.
retargetTo
  :: forall r
   . String
  -> Array { x :: Number, y :: Number }
  -> WASMSim r
  -> Effect Boolean
retargetTo name points sim = do
  ids <- Ref.read sim.forceIds
  case Map.lookup name ids of
    Just force -> FFI.setForceTargetsXY sim.wasmSim force (Array.concatMap (\point -> [ point.x, point.y ]) points)
    Nothing -> pure false

-- | Give a Collide, Radial or Contain force new radii, by the name it has in
-- | the setup, without re-applying the setup. Cheap enough to call every
-- | tick. For a collision that grows in as nodes arrive, or shrinks to let
-- | them pass. False if the setup has no such force of that name.
reradius
  :: forall r
   . String
  -> (SimulationNode r -> Number)
  -> WASMSim r
  -> Effect Boolean
reradius name radius sim = do
  ids <- Ref.read sim.forceIds
  nodes <- Ref.read sim.nodesRef
  case Map.lookup name ids of
    Just force -> FFI.setForceRadii sim.wasmSim force (map radius nodes)
    Nothing -> pure false

-- | Record everything that changes as the simulation runs: positions,
-- | velocities, pins, alpha and the other parameters, and the state of the
-- | stream that separates coincident nodes.
snapshot :: forall r. WASMSim r -> Effect FFI.Snapshot
snapshot sim = do
  -- A pin set on the nodes since the last tick belongs in the record.
  nodes <- Ref.read sim.nodesRef
  FFI.syncPinsToWasm sim.wasmSim nodes
  FFI.snapshot sim.wasmSim

-- | Put the simulation back in a recorded state: with the same setup and
-- | links, the ticks that follow are exactly the ticks that followed the
-- | snapshot. False, changing nothing, if the snapshot does not fit (it was
-- | taken with a different number of nodes, or is malformed).
restore :: forall r. FFI.Snapshot -> WASMSim r -> Effect Boolean
restore recorded sim = do
  nodes <- Ref.read sim.nodesRef
  before <- FFI.snapshot sim.wasmSim
  ok <- FFI.restore sim.wasmSim recorded
  count <- FFI.nodeCount sim.wasmSim
  if ok && count == Array.length nodes then do
    updated <- FFI.syncNodesFromWasm sim.wasmSim nodes
    -- The pins come back too: the nodes are where the next tick reads them.
    Ref.write (FFI.withPinsFromSnapshot recorded updated) sim.nodesRef
    pure true
  else do
    void $ FFI.restore sim.wasmSim before
    pure false

-- | Kinetic energy after the last tick (unit mass per node). Falls toward 0
-- | as the layout settles.
kineticEnergy :: forall r. WASMSim r -> Effect Number
kineticEnergy sim = FFI.kineticEnergy sim.wasmSim

-- | The furthest any node moved during the last tick.
maxDisplacement :: forall r. WASMSim r -> Effect Number
maxDisplacement sim = FFI.maxDisplacement sim.wasmSim

-- | The largest change in any node's step between the last two ticks: the
-- | acceleration it actually underwent.
-- |
-- | This is what `kineticEnergy` and `maxDisplacement` cannot see. A node on
-- | a spring is momentarily still at the far end of every swing, and nodes
-- | released together swing in step, so both of those vanish there, far from
-- | rest. The acceleration is then at its largest. 0 until two ticks have
-- | been made since the simulation was created, restored or resized.
maxAcceleration :: forall r. WASMSim r -> Effect Number
maxAcceleration sim = FFI.maxAcceleration sim.wasmSim

-- | Is the simulation at rest? True when, over the last two ticks, no node
-- | moved further than `step` in a tick and no node's step changed by more
-- | than `acceleration`. False until two ticks have been made.
-- |
-- | It is measured from positions, so it is right whatever moved a node, and
-- | a node held against a hard region by a force that keeps pushing counts
-- | as at rest. It says what the last two ticks did: after `retarget`, tick
-- | before asking.
-- |
-- | For a node on a spring of stiffness k (a `towards` of that strength, at
-- | alpha 1) with velocity retention r, the node is then within
-- | (acceleration + (1 - r) * (step + acceleration)) / (k * r) of where it
-- | will stop, and gets no further away. Stop on the first tick it is true.
settled :: forall r. { step :: Number, acceleration :: Number } -> WASMSim r -> Effect Boolean
settled tolerance sim = FFI.settled sim.wasmSim tolerance.step tolerance.acceleration

-- =============================================================================
-- Direct Operations
-- =============================================================================

-- | Run a single simulation tick and return the nodes where it left them.
-- | (`tick` returns the new alpha; `tick` followed by `getNodes` copies the
-- | positions out of the kernel twice.)
step :: forall r. WASMSim r -> Effect (Array (SimulationNode r))
step sim = do
  nodes <- Ref.read sim.nodesRef
  FFI.syncPinsToWasm sim.wasmSim nodes
  _ <- FFI.tick sim.wasmSim
  updatedNodes <- FFI.syncNodesFromWasm sim.wasmSim nodes
  Ref.write updatedNodes sim.nodesRef
  pure updatedNodes

-- | Run a single simulation tick. Returns new alpha value.
tick :: forall r. WASMSim r -> Effect Number
tick sim = do
  -- A pin set or cleared on the nodes since the last tick (a drag does
  -- both) has to reach the kernel first.
  nodes <- Ref.read sim.nodesRef
  FFI.syncPinsToWasm sim.wasmSim nodes
  alpha <- FFI.tick sim.wasmSim

  -- Sync positions and velocities back to PureScript
  updatedNodes <- FFI.syncNodesFromWasm sim.wasmSim nodes
  Ref.write updatedNodes sim.nodesRef

  pure alpha

-- | Run multiple ticks. Returns final alpha.
tickN :: forall r. Int -> WASMSim r -> Effect Number
tickN n sim = do
  nodes <- Ref.read sim.nodesRef
  FFI.syncPinsToWasm sim.wasmSim nodes
  alpha <- FFI.tickN sim.wasmSim n

  -- Sync final positions and velocities back
  updatedNodes <- FFI.syncNodesFromWasm sim.wasmSim nodes
  Ref.write updatedNodes sim.nodesRef

  pure alpha

-- | The nodes as the last tick (or the last change made through this
-- | module) left them. The same records until the next tick, so a pin set
-- | on one of them is seen by that tick.
getNodes :: forall r. WASMSim r -> Effect (Array (SimulationNode r))
getNodes sim = Ref.read sim.nodesRef

-- | Get current alpha value
getAlpha :: forall r. WASMSim r -> Effect Number
getAlpha sim = FFI.getAlpha sim.wasmSim

-- | Reheat simulation (set alpha to 1.0)
reheat :: forall r. WASMSim r -> Effect Unit
reheat sim = FFI.reheat sim.wasmSim

-- | Check if simulation is still running
isRunning :: forall r. WASMSim r -> Effect Boolean
isRunning sim = FFI.isRunning sim.wasmSim

-- =============================================================================
-- Pins and positions, as the default engine has them
-- =============================================================================

-- | Pin every node where it is (fx = x, fy = y), on the nodes and in the
-- | kernel.
pinNodes :: forall r. WASMSim r -> Effect Unit
pinNodes sim = do
  pinNodesInPlace_ sim.nodesRef
  nodes <- Ref.read sim.nodesRef
  FFI.syncPinsToWasm sim.wasmSim nodes

-- | Unpin every node, on the nodes and in the kernel.
unpinNodes :: forall r. WASMSim r -> Effect Unit
unpinNodes sim = do
  unpinNodesInPlace_ sim.nodesRef
  nodes <- Ref.read sim.nodesRef
  FFI.syncPinsToWasm sim.wasmSim nodes

-- | Put every node that both maps name at `start + (target - start) *
-- | progress`, on the nodes and in the kernel, so that the next tick goes
-- | on from there. Velocities and pins are left as they are.
interpolatePositions :: forall r. PositionMap -> PositionMap -> Number -> WASMSim r -> Effect Unit
interpolatePositions start target progress sim = do
  interpolatePositionsInPlace_ start target progress sim.nodesRef
  nodes <- Ref.read sim.nodesRef
  FFI.syncNodesToWasm sim.wasmSim nodes
