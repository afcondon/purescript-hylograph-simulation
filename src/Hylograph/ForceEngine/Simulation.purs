-- | Force Engine Simulation
-- |
-- | High-level API for running force simulations.
-- | This module provides a clean interface for:
-- | - Creating and configuring simulations
-- | - Running the animation loop
-- | - Handling tick callbacks for rendering
-- |
-- | The forces are PureScript (`hylograph-forces-pure`: d3-force's seven,
-- | bit for bit, and the Rust kernel's additions). There is no D3 here. Each
-- | simulation has its own seeded generator for separating coincident nodes,
-- | so the same nodes, links and forces give the same layout every time.
-- |
-- | == IMPORTANT: forces read their values when they are initialized
-- |
-- | A force evaluates its per-node and per-link values (targets, radii,
-- | strengths, filters) once, when it is given its nodes. If you update
-- | `node.gridX` and call `reheat`, a force whose target reads `gridX` still
-- | has the old values. `setNodes`, `setLinks`, `reinitializeForces` and
-- | `updateGridXYAndReinit` make every force again from its recipe.
module Hylograph.ForceEngine.Simulation
  ( -- * Node Types
    SimulationNode
  , NodeID
  , D3_ID
  , D3_XY
  , D3_VxyFxy
  , D3_FocusXY
    -- * Link Types
  , Link
  , SwizzledLink
    -- * Simulation Types
  , Simulation
  , SimConfig
  , defaultConfig
    -- * Lifecycle
  , create
  , createWithCallbacks
  , setNodes
  , setLinks
  , addForce
  , addForceRecipe
  , addForceHandle
  , removeForce
  , clearForces
  , reinitializeForces
    -- * Running
  , start
  , stop
  , tick
  , reheat
    -- * Callbacks (legacy single-callback API)
  , onTick
    -- * Callbacks (new multi-callback API)
  , setCallbacks
  , getCallbacks
    -- * Query
  , isRunning
  , getAlpha
  , getNodes
  , getLinks
    -- * Position Updates (for transitions)
  , PositionMap
  , updatePositionsInPlace
  , interpolatePositionsInPlace
  , pinNodesInPlace
  , unpinNodesInPlace
  , pinNodesAtPositions
    -- * Grid Position Updates (for ForceXGrid/ForceYGrid)
  , updateGridPositionsInPlace
  , updateGridXWithFn
  , updateGridYWithFn
  , updateGridXYAndReinit
    -- * Drag Behavior
  , attachDrag
  , attachGroupDrag
  , attachPinningDrag
    -- * DOM Utilities
  , querySelectorElements
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Foreign.Object (Object)
import Effect (Effect)
import Effect.Exception (throw)
import Type.Row (type (+))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Hylograph.ForceEngine.Core as Core
import Hylograph.ForceEngine.Types (ForceSpec(..), defaultSimParams)
import Hylograph.ForceEngine.Events (SimulationCallbacks)
import Web.DOM.Element (Element)

-- =============================================================================
-- Types - Node
-- =============================================================================

-- | Node ID type (Int for efficient lookups)
type NodeID = Int

-- | Composable row types for building simulation nodes
-- | These can be combined with (+) to create custom node types
type D3_ID      row = ( id :: NodeID | row )
type D3_XY      row = ( x :: Number, y :: Number | row )
type D3_VxyFxy  row = ( vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | row )
type D3_FocusXY row = ( cluster :: Int, focusX :: Number, focusY :: Number | row )

-- | A simulation node with all required fields for force simulation and transitions.
-- | Extends user data row with id, position (x/y), velocity (vx/vy), and fixed position (fx/fy).
-- |
-- | Example:
-- | ```purescript
-- | type MyNode = SimulationNode (name :: String, group :: Int)
-- | -- Expands to: { id :: Int, x, y, vx, vy, fx, fy, name :: String, group :: Int }
-- | ```
type SimulationNode r = Record (D3_ID + D3_XY + D3_VxyFxy + r)

-- =============================================================================
-- Types - Link
-- =============================================================================

-- | Row-polymorphic link type parameterized by ID type
-- |
-- | Example:
-- | ```purescript
-- | type MyLink = Link Int (value :: Number)
-- | -- Expands to: { source :: Int, target :: Int, value :: Number }
-- | ```
type Link id r = { source :: id, target :: id | r }

-- | Swizzled link where source/target are node object references
type SwizzledLink nodeData r =
  { source :: SimulationNode nodeData
  , target :: SimulationNode nodeData
  | r
  }

-- | A running simulation
-- | This is a mutable structure that holds the simulation state
type Simulation row linkRow =
  { nodes :: Ref (Array (SimulationNode row))
  , links :: Ref (Array { source :: Int, target :: Int | linkRow })
  , forces :: Ref (Map String (Core.ForceHandle row))
  , random :: Core.RandomSource  -- separates coincident nodes; seeded, one per simulation
  , alpha :: Ref Number
  , prevAlpha :: Ref Number  -- For alpha threshold detection
  , config :: SimConfig
  , running :: Ref Boolean
  , cancelAnimation :: Ref (Effect Unit)
  , tickCallback :: Ref (Effect Unit)  -- Legacy single callback
  , callbacks :: Maybe SimulationCallbacks  -- New multi-callback system
  }

-- | Simulation configuration
type SimConfig =
  { alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  , velocityRetention :: Number
  }

-- | Default simulation configuration
defaultConfig :: SimConfig
defaultConfig =
  { alphaMin: defaultSimParams.alphaMin
  , alphaDecay: defaultSimParams.alphaDecay
  , alphaTarget: defaultSimParams.alphaTarget
  , velocityRetention: defaultSimParams.velocityRetention
  }

-- =============================================================================
-- Lifecycle
-- =============================================================================

-- | Create a new simulation (without callbacks)
-- |
-- | For the new callback-based API, use `createWithCallbacks` instead.
create :: forall row linkRow. SimConfig -> Effect (Simulation row linkRow)
create config = do
  nodesRef <- Ref.new []
  linksRef <- Ref.new []
  forcesRef <- Ref.new Map.empty
  alphaRef <- Ref.new 1.0
  prevAlphaRef <- Ref.new 1.0
  runningRef <- Ref.new false
  cancelRef <- Ref.new (pure unit)
  tickRef <- Ref.new (pure unit)
  random <- Core.randomSource
  pure
    { nodes: nodesRef
    , links: linksRef
    , forces: forcesRef
    , random
    , alpha: alphaRef
    , prevAlpha: prevAlphaRef
    , config
    , running: runningRef
    , cancelAnimation: cancelRef
    , tickCallback: tickRef
    , callbacks: Nothing
    }

-- | Create a new simulation with callback system
-- |
-- | Example:
-- | ```purescript
-- | callbacks <- defaultCallbacks
-- | onSimulationTick updateDOM callbacks
-- | onSimulationStop (log "Simulation settled") callbacks
-- | sim <- createWithCallbacks config callbacks
-- | ```
createWithCallbacks :: forall row linkRow.
  SimConfig
  -> SimulationCallbacks
  -> Effect (Simulation row linkRow)
createWithCallbacks config cbs = do
  nodesRef <- Ref.new []
  linksRef <- Ref.new []
  forcesRef <- Ref.new Map.empty
  alphaRef <- Ref.new 1.0
  prevAlphaRef <- Ref.new 1.0
  runningRef <- Ref.new false
  cancelRef <- Ref.new (pure unit)
  tickRef <- Ref.new (pure unit)
  random <- Core.randomSource
  pure
    { nodes: nodesRef
    , links: linksRef
    , forces: forcesRef
    , random
    , alpha: alphaRef
    , prevAlpha: prevAlphaRef
    , config
    , running: runningRef
    , cancelAnimation: cancelRef
    , tickCallback: tickRef
    , callbacks: Just cbs
    }

-- | Set the nodes for the simulation
-- | This initializes the nodes and makes every force again for them
setNodes :: forall row linkRow.
  Array (SimulationNode row)
  -> Simulation row linkRow
  -> Effect Unit
setNodes nodes sim = do
  -- Sets index, pins, default velocity, and a place for a node without one
  Core.initializeNodes nodes
  Ref.write nodes sim.nodes
  reinitializeForces sim

-- | Set the links for the simulation
-- | This makes every force again, so that link forces see the new links
setLinks :: forall row linkRow.
  Array { source :: Int, target :: Int | linkRow }
  -> Simulation row linkRow
  -> Effect Unit
setLinks links sim = do
  Ref.write links sim.links
  reinitializeForces sim

-- | Make every force again from its recipe, for the simulation's current
-- | nodes and links. Needed after anything a force's per-node or per-link
-- | values are computed from has changed.
-- |
-- | Throws, changing nothing, if a link names a node that is not among the
-- | nodes (`node not found: <id>`).
reinitializeForces :: forall row linkRow. Simulation row linkRow -> Effect Unit
reinitializeForces sim = do
  nodes <- Ref.read sim.nodes
  links <- Ref.read sim.links
  forces <- Ref.read sim.forces
  case traverse (Core.reinitializeForce sim.random nodes (linkIds links)) forces of
    Left problem -> throw ("Hylograph.ForceEngine.Simulation: " <> problem)
    Right made -> Ref.write made sim.forces

-- | The links as the forces want them: the two ids and nothing else.
linkIds :: forall linkRow. Array { source :: Int, target :: Int | linkRow } -> Core.Links
linkIds = map \link -> { source: link.source, target: link.target }

-- | Add a force to the simulation
addForce :: forall row linkRow.
  ForceSpec
  -> Simulation row linkRow
  -> Effect Unit
addForce spec sim = addForceRecipe name recipe sim
  where
  name = case spec of
    ManyBody n _ -> n
    Collide n _ -> n
    Link n _ -> n
    Center n _ -> n
    PositionX n _ -> n
    PositionY n _ -> n
    Radial n _ -> n
  recipe = case spec of
    ManyBody _ config -> Core.createManyBody config
    Collide _ config -> Core.createCollide config
    Link _ config -> Core.createLink config
    Center _ config -> Core.createCenter config
    PositionX _ config -> Core.createForceX config
    PositionY _ config -> Core.createForceY config
    Radial _ config -> Core.createRadial config

-- | Add a force from its recipe, under a name (used for later removal). A
-- | force already there under that name is replaced.
-- |
-- | Throws, changing nothing, if a link names a node that is not among the
-- | nodes.
addForceRecipe :: forall row linkRow.
  String
  -> Core.ForceRecipe row
  -> Simulation row linkRow
  -> Effect Unit
addForceRecipe name recipe sim = do
  nodes <- Ref.read sim.nodes
  links <- Ref.read sim.links
  case Core.initializeForce sim.random nodes (linkIds links) recipe of
    Left problem -> throw ("Hylograph.ForceEngine.Simulation: " <> problem)
    Right handle -> Ref.modify_ (Map.insert name handle) sim.forces

-- | Deprecated: the old name of `addForceRecipe`. A force used to be handed
-- | over as a handle that had already been initialized; it is now handed
-- | over as a recipe and initialized here, so a call to
-- | `Core.initializeForce` before this one is no longer needed.
addForceHandle :: forall row linkRow.
  String
  -> Core.ForceRecipe row
  -> Simulation row linkRow
  -> Effect Unit
addForceHandle = addForceRecipe

-- | Remove a force from the simulation
removeForce :: forall row linkRow.
  String
  -> Simulation row linkRow
  -> Effect Unit
removeForce name sim = do
  Ref.modify_ (Map.delete name) sim.forces

-- | Clear all forces from the simulation
-- | Used when switching between force configurations (e.g., grid vs tree vs orbit)
clearForces :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
clearForces sim = do
  Ref.write Map.empty sim.forces

-- =============================================================================
-- Running
-- =============================================================================

-- | Start the simulation animation loop
start :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
start sim = do
  -- Don't start if already running
  alreadyRunning <- Ref.read sim.running
  unless alreadyRunning do
    Ref.write true sim.running

    -- Fire onStart callback
    invokeStartCallback sim

    -- Start animation loop
    cancel <- Core.startAnimation \_ -> do
      running <- Ref.read sim.running
      if running
        then do
          newAlpha <- tick sim
          -- Continue if alpha > 0, otherwise mark as stopped
          let shouldContinue = newAlpha > 0.0
          unless shouldContinue do
            Ref.write false sim.running
            -- Fire onStop callback
            invokeStopCallback sim
          pure shouldContinue
        else pure false

    Ref.write cancel sim.cancelAnimation

-- | Stop the simulation
stop :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
stop sim = do
  Ref.write false sim.running
  cancel <- Ref.read sim.cancelAnimation
  cancel

-- | Run a single tick of the simulation
-- | Returns the new alpha value
tick :: forall row linkRow.
  Simulation row linkRow
  -> Effect Number
tick sim = do
  nodes <- Ref.read sim.nodes
  forces <- Ref.read sim.forces
  alpha <- Ref.read sim.alpha
  prevAlpha <- Ref.read sim.prevAlpha

  -- Apply all forces, in order of their names
  let forceHandles = Array.fromFoldable $ Map.values forces
  Core.applyForces forceHandles alpha

  -- Integrate positions
  Core.integratePositions nodes sim.config.velocityRetention

  -- Hard containment, then pins again
  Core.constrain forceHandles nodes

  -- Decay alpha
  let newAlpha = Core.decayAlpha alpha sim.config.alphaMin sim.config.alphaDecay sim.config.alphaTarget
  Ref.write newAlpha sim.alpha
  Ref.write alpha sim.prevAlpha  -- Store current as previous for next tick

  -- Call legacy tick callback
  legacyCallback <- Ref.read sim.tickCallback
  legacyCallback

  -- Call new tick callback (if callbacks are configured)
  invokeTickCallback sim

  -- Check for alpha threshold crossings
  checkAlphaThresholds prevAlpha newAlpha sim

  pure newAlpha

-- | Reheat the simulation (set alpha to 1)
reheat :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
reheat sim = do
  Ref.write 1.0 sim.alpha
  -- If not running, start
  running <- Ref.read sim.running
  unless running $ start sim

-- =============================================================================
-- Callbacks
-- =============================================================================

-- | Set the tick callback (legacy single-callback API)
-- | This is called after each simulation tick
onTick :: forall row linkRow.
  Effect Unit
  -> Simulation row linkRow
  -> Effect Unit
onTick callback sim = do
  Ref.write callback sim.tickCallback

-- | Set the callbacks for a simulation (new multi-callback API)
-- | Note: This replaces any callbacks passed to createWithCallbacks
setCallbacks :: forall row linkRow.
  SimulationCallbacks
  -> Simulation row linkRow
  -> Simulation row linkRow
setCallbacks cbs sim = sim { callbacks = Just cbs }

-- | Get the current callbacks (if any)
getCallbacks :: forall row linkRow.
  Simulation row linkRow
  -> Maybe SimulationCallbacks
getCallbacks sim = sim.callbacks

-- =============================================================================
-- Internal Callback Helpers
-- =============================================================================

-- | Invoke the onStart callback if present
invokeStartCallback :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
invokeStartCallback sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    callback <- Ref.read cbs.onStart
    callback

-- | Invoke the onStop callback if present
invokeStopCallback :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
invokeStopCallback sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    callback <- Ref.read cbs.onStop
    callback

-- | Invoke the onTick callback if present
invokeTickCallback :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
invokeTickCallback sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    callback <- Ref.read cbs.onTick
    callback

-- | Check if alpha crossed any thresholds and invoke callback
-- | Thresholds: 0.5, 0.1, 0.01
checkAlphaThresholds :: forall row linkRow.
  Number  -- ^ Previous alpha
  -> Number  -- ^ New alpha
  -> Simulation row linkRow
  -> Effect Unit
checkAlphaThresholds prevAlpha newAlpha sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    -- Check each threshold
    for_ alphaThresholds \threshold ->
      when (crossedThreshold prevAlpha newAlpha threshold) do
        callback <- Ref.read cbs.onAlphaThreshold
        callback newAlpha
  where
  alphaThresholds = [0.5, 0.1, 0.01]
  crossedThreshold prev new thresh = prev > thresh && new <= thresh

-- =============================================================================
-- Query
-- =============================================================================

-- | Check if the simulation is running
isRunning :: forall row linkRow.
  Simulation row linkRow
  -> Effect Boolean
isRunning sim = Ref.read sim.running

-- | Get the current alpha value
getAlpha :: forall row linkRow.
  Simulation row linkRow
  -> Effect Number
getAlpha sim = Ref.read sim.alpha

-- | Get the current nodes (with updated positions)
getNodes :: forall row linkRow.
  Simulation row linkRow
  -> Effect (Array (SimulationNode row))
getNodes sim = Ref.read sim.nodes

-- | Get the current links from the simulation
-- | Note: These are the raw links with source/target as Int IDs.
-- | For swizzled links (with node references), use swizzleLinksByIndex from Links module.
getLinks :: forall row linkRow.
  Simulation row linkRow
  -> Effect (Array { source :: Int, target :: Int | linkRow })
getLinks sim = Ref.read sim.links

-- =============================================================================
-- Position Updates (for tick-driven transitions)
-- =============================================================================

-- | Position map type (keyed by node id as string)
type PositionMap = Object { x :: Number, y :: Number }

-- FFI imports (typed to ensure SimulationNode structure is present)
foreign import updatePositionsInPlace_
  :: forall row. PositionMap -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import interpolatePositionsInPlace_
  :: forall row. PositionMap -> PositionMap -> Number -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import pinNodesInPlace_
  :: forall row. Ref (Array (SimulationNode row)) -> Effect Unit

foreign import unpinNodesInPlace_
  :: forall row. Ref (Array (SimulationNode row)) -> Effect Unit

foreign import pinNodesAtPositions_
  :: forall row. PositionMap -> Ref (Array (SimulationNode row)) -> Effect Unit

-- | Update node positions in place from a position map
-- | Mutates the simulation's internal nodes (the same objects the DOM is bound to)
updatePositionsInPlace :: forall row linkRow.
  PositionMap
  -> Simulation row linkRow
  -> Effect Unit
updatePositionsInPlace positions sim =
  updatePositionsInPlace_ positions sim.nodes

-- | Interpolate node positions in place between start and target
-- | Progress should be 0.0 to 1.0 (apply easing before calling)
interpolatePositionsInPlace :: forall row linkRow.
  PositionMap  -- ^ Start positions
  -> PositionMap  -- ^ Target positions
  -> Number  -- ^ Progress (0-1, already eased)
  -> Simulation row linkRow
  -> Effect Unit
interpolatePositionsInPlace startPos targetPos progress sim =
  interpolatePositionsInPlace_ startPos targetPos progress sim.nodes

-- | Pin all nodes at their current positions (fx = x, fy = y)
-- | Use before starting a transition to freeze current state
pinNodesInPlace :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
pinNodesInPlace sim = pinNodesInPlace_ sim.nodes

-- | Unpin all nodes (fx = null, fy = null)
-- | Use after transition to Grid scene to let forces take over
unpinNodesInPlace :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
unpinNodesInPlace sim = unpinNodesInPlace_ sim.nodes

-- | Pin nodes at specific positions from a position map
-- | Sets both x/y and fx/fy - use at end of transition to non-Grid scenes
pinNodesAtPositions :: forall row linkRow.
  PositionMap
  -> Simulation row linkRow
  -> Effect Unit
pinNodesAtPositions positions sim =
  pinNodesAtPositions_ positions sim.nodes

-- =============================================================================
-- Grid Position Updates (for ForceXGrid/ForceYGrid forces)
-- =============================================================================

-- FFI for grid position updates
foreign import updateGridPositionsInPlace_
  :: forall row. PositionMap -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import updateGridXWithFn_
  :: forall row. (SimulationNode row -> Number) -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import updateGridYWithFn_
  :: forall row. (SimulationNode row -> Number) -> Ref (Array (SimulationNode row)) -> Effect Unit

-- | Update node gridX/gridY positions in place from a positions map
-- | Used to change force targets before reheating the simulation
-- | The positions map is keyed by node.id (as string) -> { x, y }
-- | where x becomes gridX and y becomes gridY
updateGridPositionsInPlace :: forall row linkRow.
  PositionMap
  -> Simulation row linkRow
  -> Effect Unit
updateGridPositionsInPlace positions sim =
  updateGridPositionsInPlace_ positions sim.nodes

-- | Update gridX for all nodes using a function
-- | Useful for toggle animations where only the X target changes
-- | Example: toggle between combined (all same X) and separated (X by department)
updateGridXWithFn :: forall row linkRow.
  (SimulationNode row -> Number)
  -> Simulation row linkRow
  -> Effect Unit
updateGridXWithFn fn sim =
  updateGridXWithFn_ fn sim.nodes

-- | Update gridY for all nodes using a function
-- | Useful for toggle animations where only the Y target changes
updateGridYWithFn :: forall row linkRow.
  (SimulationNode row -> Number)
  -> Simulation row linkRow
  -> Effect Unit
updateGridYWithFn fn sim =
  updateGridYWithFn_ fn sim.nodes

-- | Update gridX and/or gridY for all nodes, make the forces again so that
-- | they read the new values, and reheat.
-- |
-- | This is the recommended way to animate force-directed transitions: a
-- | force reads its targets when it is initialized, so updating
-- | `node.gridX` alone changes nothing.
-- |
-- | Example:
-- | ```purescript
-- | -- Move nodes to new X positions (Y unchanged)
-- | updateGridXYAndReinit (Just (\node -> departmentX node.department)) Nothing sim
-- | ```
-- |
-- | (Until 2026-11-18 this also took the handles of the forces to
-- | re-initialize; it now re-initializes them all.)
updateGridXYAndReinit :: forall row linkRow.
  Maybe (SimulationNode row -> Number)  -- ^ gridX update function (Nothing = don't update X)
  -> Maybe (SimulationNode row -> Number)  -- ^ gridY update function (Nothing = don't update Y)
  -> Simulation row linkRow
  -> Effect Unit
updateGridXYAndReinit mXFn mYFn sim = do
  for_ mXFn \xFn ->
    updateGridXWithFn_ xFn sim.nodes
  for_ mYFn \yFn ->
    updateGridYWithFn_ yFn sim.nodes
  reinitializeForces sim
  reheat sim

-- =============================================================================
-- Drag Behavior
-- =============================================================================

-- | Attach simulation-aware drag behavior to node elements
-- |
-- | This sets up pointer handlers (native Pointer Events, no D3) that:
-- | 1. Reheat the simulation on drag start
-- | 2. Update fx/fy (fixed position) during drag
-- | 3. Clear fx/fy on drag end (release node)
-- |
-- | Example:
-- | ```purescript
-- | elements <- select ".node" >>= selectAll
-- | attachDrag elements sim
-- | ```
attachDrag :: forall row linkRow.
  Array Element
  -> Simulation row linkRow
  -> Effect Unit
attachDrag elements sim =
  Core.attachDragWithReheat elements (reheat sim)

-- | Attach drag to transformed group elements (like bubble packs)
-- |
-- | Unlike `attachDrag`, this version takes a container selector to
-- | get pointer coordinates in the correct coordinate space. Use this when
-- | dragging `<g>` elements that have `transform` attributes.
-- |
-- | Example:
-- | ```purescript
-- | -- For bubble packs inside a zoom group
-- | packElements <- select ".module-pack" >>= selectAll
-- | attachGroupDrag packElements "#zoom-group" sim
-- | ```
attachGroupDrag :: forall row linkRow.
  Array Element
  -> String  -- ^ Container selector (e.g., "#zoom-group")
  -> Simulation row linkRow
  -> Effect Unit
attachGroupDrag elements containerSelector sim =
  Core.attachGroupDragWithReheat elements containerSelector (reheat sim)

-- | Attach drag with toggle-pinning behavior
-- |
-- | Unlike `attachDrag`, this version keeps nodes pinned after drag ends.
-- | - First drag: pins the node where you drop it
-- | - Subsequent drags: if you barely move (< 3px), unpins the node
-- |
-- | This is useful for force playgrounds and exploration where users want
-- | to "fix" certain nodes in place while letting others float freely.
-- |
-- | Example:
-- | ```purescript
-- | nodeElements <- select ".node" >>= selectAll
-- | attachPinningDrag nodeElements sim
-- | ```
attachPinningDrag :: forall row linkRow.
  Array Element
  -> Simulation row linkRow
  -> Effect Unit
attachPinningDrag elements sim =
  Core.attachPinningDrag elements (reheat sim)

-- =============================================================================
-- DOM Utilities
-- =============================================================================

-- | Query DOM elements by CSS selector
-- |
-- | Convenience re-export from Core for getting element references.
-- |
-- | Example:
-- | ```purescript
-- | nodeCircles <- querySelectorElements "#my-graph circle"
-- | attachPinningDrag nodeCircles sim
-- | ```
querySelectorElements :: String -> Effect (Array Element)
querySelectorElements = Core.querySelectorElements
