-- | WASM Force Engine
-- |
-- | High-performance force simulation using Rust/WebAssembly.
-- | Faster than the default (PureScript) kernel; the README has the
-- | measurement.
-- |
-- | Usage:
-- | ```purescript
-- | import Hylograph.ForceEngine.WASM as WASM
-- |
-- | main = do
-- |   -- Initialize WASM module (once at app startup)
-- |   WASM.initWasm "./pkg/force_kernel.js"
-- |
-- |   -- Create simulation
-- |   sim <- WASM.create 1000
-- |
-- |   -- Configure forces
-- |   WASM.configureManyBody sim { strength: -30.0, theta: 0.9, distanceMin: 1.0, distanceMax: 10000.0 }
-- |   WASM.configureLinks sim { distance: 30.0, strength: 1.0, iterations: 1 }
-- |   WASM.configureCenter sim { x: 0.0, y: 0.0, strength: 1.0 }
-- |
-- |   -- Set links
-- |   WASM.setLinks sim [0, 1, 1, 2, 2, 3]  -- [source, target, source, target, ...]
-- |
-- |   -- Run simulation
-- |   WASM.tick sim
-- |
-- |   -- Get results
-- |   positions <- WASM.getPositions sim  -- [x0, y0, x1, y1, ...]
-- | ```
module Hylograph.ForceEngine.WASM
  ( -- * Types
    WASMSimulation
  , ManyBodyConfig
  , LinkConfig
  , CenterConfig
  , CollideConfig
  , PositionConfig
  , RadialConfig
  , ForceId
    -- * Initialization
  , initWasm
  , isWasmReady
    -- * Simulation Lifecycle
  , create
  , free
  , setNodeCount
    -- * Node Operations
  , setPosition
  , setAllPositions
  , getPositions
  , getPositionsAsObjects
  , fixNode
  , unfixNode
    -- * Link Operations
  , setLinks
  , setLinksFromRecords
    -- * Force Configuration
  , configureManyBody
  , configureLinks
  , configureCenter
  , enableForces
  , configureCollide
  , configureForceX
  , configureForceY
  , enableForcesAll
    -- * Force List (any number of forces, per-node values, filters)
  , clearForces
  , forceCount
  , addManyBody
  , addLinks
  , addCenter
  , addForceX
  , addForceY
  , addCollide
  , addRadial
  , setForceStrengths
  , setForceRadii
  , setForceTargets
  , setForceDistances
  , setForceMask
    -- * Forces d3-force does not have
  , ManyBodyScope(..)
  , addTowards
  , setForceTargetsXY
  , addRectCollide
  , setForceHalfWidths
  , setForceHalfHeights
  , addContainment
  , containmentAddRect
  , containmentAddCircle
  , containmentAddPolygon
  , setForceRegions
  , addCohesion
  , setForceGroups
  , setManyBodyScope
  , addDirectional
  , setForceGaps
    -- * Snapshot, restore, convergence
  , Snapshot
  , snapshot
  , restore
  , kineticEnergy
  , maxDisplacement
  , maxAcceleration
  , settled
    -- * Collision that yields
  , setCollideAlphaScaled
  , setCollideFilter
    -- * Simulation Control
  , setAlpha
  , getAlpha
  , setAlphaDecay
  , setAlphaMin
  , setAlphaTarget
  , setVelocityRetention
  , setVelocityDecay
  , resetRandom
  , reheat
  , tick
  , tickN
  , isRunning
    -- * Queries
  , nodeCount
  , linkCount
    -- * Node Array Sync
  , syncPositionsToWasm
  , syncPositionsFromWasm
  , syncNodesToWasm
  , syncPinsToWasm
  , withPinsFromSnapshot
  , syncNodesFromWasm
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, EffectFn7, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn6, runEffectFn7)


-- =============================================================================
-- Types
-- =============================================================================

-- | Opaque type for WASM simulation handle
foreign import data WASMSimulation :: Type

-- | Many-body force configuration
type ManyBodyConfig =
  { strength :: Number     -- Positive = attract, negative = repel (default: -30)
  , theta :: Number        -- Barnes-Hut approximation parameter (default: 0.9)
  , distanceMin :: Number  -- Minimum distance (default: 1.0)
  , distanceMax :: Number  -- Maximum distance (default: Infinity)
  }

-- | Link force configuration
type LinkConfig =
  { distance :: Number     -- Target distance (default: 30)
  , strength :: Number     -- Spring strength 0-1 (default: 1.0)
  , iterations :: Int      -- Iterations per tick (default: 1)
  }

-- | Center force configuration
type CenterConfig =
  { x :: Number            -- Center x (default: 0)
  , y :: Number            -- Center y (default: 0)
  , strength :: Number     -- Strength (default: 1.0)
  }

-- | Collide force configuration
type CollideConfig =
  { radius :: Number       -- Collision radius
  , strength :: Number     -- How strongly overlaps are resolved, 0-1
  , iterations :: Int      -- Iterations per tick
  }

-- | Configuration of a force pulling toward one coordinate (forceX, forceY)
type PositionConfig =
  { target :: Number       -- The x (or y) nodes are pulled toward
  , strength :: Number     -- How strongly, 0-1
  }

-- | Radial force configuration
type RadialConfig =
  { radius :: Number       -- Radius of the circle nodes are pulled toward
  , x :: Number            -- Centre x
  , y :: Number            -- Centre y
  , strength :: Number     -- How strongly, 0-1
  }

-- | Everything that changes as a simulation runs (positions, velocities,
-- | pins, alpha and the other parameters, the state of the stream that
-- | nudges coincident nodes apart), as recorded by `snapshot`.
foreign import data Snapshot :: Type

-- | Which pairs of nodes a many-body force acts between.
data ManyBodyScope
  = AllPairs
  | WithinGroups
  | BetweenGroups

derive instance eqManyBodyScope :: Eq ManyBodyScope

-- | One force in the kernel's force list, as returned by the `add*`
-- | functions. Valid until the next `clearForces`.
newtype ForceId = ForceId Int

derive newtype instance eqForceId :: Eq ForceId
derive newtype instance showForceId :: Show ForceId


-- =============================================================================
-- FFI Declarations
-- =============================================================================

foreign import initWasm_ :: String -> EffectFnAff Unit
foreign import isWasmReady_ :: Effect Boolean
foreign import createSimulation_ :: Int -> Effect WASMSimulation
foreign import freeSimulation_ :: WASMSimulation -> Effect Unit
foreign import setNodeCount_ :: WASMSimulation -> Int -> Effect Unit
foreign import setLinks_ :: WASMSimulation -> Array Int -> Effect Unit
foreign import setPosition_ :: WASMSimulation -> Int -> Number -> Number -> Effect Unit
foreign import setAllPositions_ :: WASMSimulation -> Array Number -> Effect Unit
foreign import getPositions_ :: WASMSimulation -> Effect (Array Number)
foreign import getPositionsAsObjects_ :: WASMSimulation -> Effect (Array { x :: Number, y :: Number })
foreign import fixNode_ :: WASMSimulation -> Int -> Number -> Number -> Effect Unit
foreign import unfixNode_ :: WASMSimulation -> Int -> Effect Unit
foreign import configureManyBody_ :: WASMSimulation -> ManyBodyConfig -> Effect Unit
foreign import configureLinks_ :: WASMSimulation -> LinkConfig -> Effect Unit
foreign import configureCenter_ :: WASMSimulation -> CenterConfig -> Effect Unit
foreign import enableForces_ :: WASMSimulation -> Boolean -> Boolean -> Boolean -> Effect Unit
foreign import setAlpha_ :: WASMSimulation -> Number -> Effect Unit
foreign import getAlpha_ :: WASMSimulation -> Effect Number
foreign import setAlphaDecay_ :: WASMSimulation -> Number -> Effect Unit
foreign import setVelocityRetention_ :: WASMSimulation -> Number -> Effect Unit
foreign import reheat_ :: WASMSimulation -> Effect Unit
foreign import tick_ :: WASMSimulation -> Effect Number
foreign import tickN_ :: WASMSimulation -> Int -> Effect Number
foreign import isRunning_ :: WASMSimulation -> Effect Boolean
foreign import nodeCount_ :: WASMSimulation -> Effect Int
foreign import linkCount_ :: WASMSimulation -> Effect Int
foreign import syncPositionsToWasm_ :: forall r. WASMSimulation -> Array { x :: Number, y :: Number | r } -> Effect Unit
foreign import syncPositionsFromWasm_ :: forall r. WASMSimulation -> Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r } -> Effect (Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r })

foreign import addTowards_ :: EffectFn2 WASMSimulation Number ForceId
foreign import setForceTargetsXY_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import addRectCollide_ :: EffectFn5 WASMSimulation Number Number Number Int ForceId
foreign import setForceHalfWidths_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import setForceHalfHeights_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import addContainment_ :: EffectFn3 WASMSimulation Number Boolean ForceId
foreign import containmentAddRect_ :: EffectFn6 WASMSimulation ForceId Number Number Number Number Int
foreign import containmentAddCircle_ :: EffectFn5 WASMSimulation ForceId Number Number Number Int
foreign import containmentAddPolygon_ :: EffectFn3 WASMSimulation ForceId (Array Number) Int
foreign import setForceRegions_ :: EffectFn3 WASMSimulation ForceId (Array Int) Boolean
foreign import addCohesion_ :: EffectFn2 WASMSimulation Number ForceId
foreign import setForceGroups_ :: EffectFn3 WASMSimulation ForceId (Array Int) Boolean
foreign import setManyBodyScope_ :: EffectFn3 WASMSimulation ForceId Int Boolean
foreign import addDirectional_ :: EffectFn6 WASMSimulation Number Number Number Number Int ForceId
foreign import setForceGaps_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import snapshot_ :: EffectFn1 WASMSimulation Snapshot
foreign import restore_ :: EffectFn2 WASMSimulation Snapshot Boolean
foreign import kineticEnergy_ :: EffectFn1 WASMSimulation Number
foreign import maxDisplacement_ :: EffectFn1 WASMSimulation Number
foreign import maxAcceleration_ :: EffectFn1 WASMSimulation Number
foreign import settled_ :: EffectFn3 WASMSimulation Number Number Boolean
foreign import setCollideAlphaScaled_ :: EffectFn3 WASMSimulation ForceId Boolean Boolean
foreign import setCollideFilter_ :: EffectFn3 WASMSimulation ForceId (Array Boolean) Boolean
foreign import clearForces_ :: EffectFn1 WASMSimulation Unit
foreign import forceCount_ :: EffectFn1 WASMSimulation Int
foreign import addManyBody_ :: EffectFn5 WASMSimulation Number Number Number Number ForceId
foreign import addLinks_ :: EffectFn4 WASMSimulation Number Number Int ForceId
foreign import addCenter_ :: EffectFn4 WASMSimulation Number Number Number ForceId
foreign import addForceX_ :: EffectFn3 WASMSimulation Number Number ForceId
foreign import addForceY_ :: EffectFn3 WASMSimulation Number Number ForceId
foreign import addCollide_ :: EffectFn4 WASMSimulation Number Number Int ForceId
foreign import addRadial_ :: EffectFn5 WASMSimulation Number Number Number Number ForceId
foreign import setForceStrengths_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import setForceRadii_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import setForceTargets_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import setForceDistances_ :: EffectFn3 WASMSimulation ForceId (Array Number) Boolean
foreign import setForceMask_ :: EffectFn3 WASMSimulation ForceId (Array Boolean) Boolean
foreign import configureCollide_ :: EffectFn4 WASMSimulation Number Number Int Unit
foreign import configureForceX_ :: EffectFn3 WASMSimulation Number Number Unit
foreign import configureForceY_ :: EffectFn3 WASMSimulation Number Number Unit
foreign import enableForcesAll_ :: EffectFn7 WASMSimulation Boolean Boolean Boolean Boolean Boolean Boolean Unit
foreign import setAlphaMin_ :: EffectFn2 WASMSimulation Number Unit
foreign import setAlphaTarget_ :: EffectFn2 WASMSimulation Number Unit
foreign import resetRandom_ :: EffectFn1 WASMSimulation Unit
foreign import syncNodesToWasm_ :: forall r. EffectFn2 WASMSimulation (Array { x :: Number, y :: Number, vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | r }) Unit
foreign import syncPinsToWasm_ :: forall r. EffectFn2 WASMSimulation (Array { fx :: Nullable Number, fy :: Nullable Number | r }) Unit
foreign import withPinsFromSnapshot_ :: forall r. Fn2 Snapshot (Array { fx :: Nullable Number, fy :: Nullable Number | r }) (Array { fx :: Nullable Number, fy :: Nullable Number | r })
foreign import syncNodesFromWasm_ :: forall r. EffectFn2 WASMSimulation (Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r }) (Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r })


-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the WASM module. Call once at application startup.
-- |
-- | The URL should point to the generated JavaScript module from wasm-pack.
-- | Example: `initWasm "./pkg/force_kernel.js"`
initWasm :: String -> Aff Unit
initWasm url = fromEffectFnAff (initWasm_ url)

-- | Check if WASM is initialized and ready
isWasmReady :: Effect Boolean
isWasmReady = isWasmReady_


-- =============================================================================
-- Simulation Lifecycle
-- =============================================================================

-- | Create a new WASM simulation with the given number of nodes.
-- | Nodes are initialized in a phyllotaxis (sunflower) pattern.
create :: Int -> Effect WASMSimulation
create = createSimulation_

-- | Free WASM simulation memory. Call when done with simulation.
free :: WASMSimulation -> Effect Unit
free = freeSimulation_

-- | Resize simulation to new node count
setNodeCount :: WASMSimulation -> Int -> Effect Unit
setNodeCount = setNodeCount_


-- =============================================================================
-- Node Operations
-- =============================================================================

-- | Set a single node's position
setPosition :: WASMSimulation -> Int -> Number -> Number -> Effect Unit
setPosition = setPosition_

-- | Set all positions from flat array [x0, y0, x1, y1, ...]
setAllPositions :: WASMSimulation -> Array Number -> Effect Unit
setAllPositions = setAllPositions_

-- | Get all positions as flat array [x0, y0, x1, y1, ...]
getPositions :: WASMSimulation -> Effect (Array Number)
getPositions = getPositions_

-- | Get positions as array of {x, y} objects
getPositionsAsObjects :: WASMSimulation -> Effect (Array { x :: Number, y :: Number })
getPositionsAsObjects = getPositionsAsObjects_

-- | Fix a node at a position (like D3's fx/fy)
fixNode :: WASMSimulation -> Int -> Number -> Number -> Effect Unit
fixNode = fixNode_

-- | Unfix a node
unfixNode :: WASMSimulation -> Int -> Effect Unit
unfixNode = unfixNode_


-- =============================================================================
-- Link Operations
-- =============================================================================

-- | Set links from flat array [source0, target0, source1, target1, ...]
setLinks :: WASMSimulation -> Array Int -> Effect Unit
setLinks = setLinks_

-- | Set links from array of records with source/target fields
setLinksFromRecords :: forall r. WASMSimulation -> Array { source :: Int, target :: Int | r } -> Effect Unit
setLinksFromRecords sim links = do
  let flatLinks = Array.concatMap (\l -> [l.source, l.target]) links
  setLinks_ sim flatLinks


-- =============================================================================
-- Force Configuration
-- =============================================================================

-- | Configure many-body (charge) force
configureManyBody :: WASMSimulation -> ManyBodyConfig -> Effect Unit
configureManyBody = configureManyBody_

-- | Configure link (spring) force
configureLinks :: WASMSimulation -> LinkConfig -> Effect Unit
configureLinks = configureLinks_

-- | Configure center force
configureCenter :: WASMSimulation -> CenterConfig -> Effect Unit
configureCenter = configureCenter_

-- | Enable/disable forces individually
enableForces :: WASMSimulation -> { manyBody :: Boolean, links :: Boolean, center :: Boolean } -> Effect Unit
enableForces sim cfg = enableForces_ sim cfg.manyBody cfg.links cfg.center

-- | Configure the built-in collide force
configureCollide :: WASMSimulation -> CollideConfig -> Effect Unit
configureCollide sim cfg = runEffectFn4 configureCollide_ sim cfg.radius cfg.strength cfg.iterations

-- | Configure the built-in forceX
configureForceX :: WASMSimulation -> PositionConfig -> Effect Unit
configureForceX sim cfg = runEffectFn3 configureForceX_ sim cfg.target cfg.strength

-- | Configure the built-in forceY
configureForceY :: WASMSimulation -> PositionConfig -> Effect Unit
configureForceY sim cfg = runEffectFn3 configureForceY_ sim cfg.target cfg.strength

-- | Enable/disable all six built-in forces. (`enableForces` reaches only
-- | three of them and leaves the other three as they were.)
enableForcesAll
  :: WASMSimulation
  -> { manyBody :: Boolean, links :: Boolean, center :: Boolean, forceX :: Boolean, forceY :: Boolean, collide :: Boolean }
  -> Effect Unit
enableForcesAll sim cfg =
  runEffectFn7 enableForcesAll_ sim cfg.manyBody cfg.links cfg.center cfg.forceX cfg.forceY cfg.collide


-- =============================================================================
-- Force List
-- =============================================================================
--
-- Beside its six built-in forces the kernel keeps a list of forces: any
-- number, of any type, applied each tick in the order they were added, after
-- the built-in ones. A force in the list can take one value per node (or per
-- link) where D3 would take an accessor function, and a filter. The kernel
-- never calls back into JavaScript: evaluate the function over the nodes
-- here and pass the resulting array.

-- | Remove every force from the force list. Built-in forces are unaffected;
-- | turn those off with `enableForcesAll`.
clearForces :: WASMSimulation -> Effect Unit
clearForces = runEffectFn1 clearForces_

-- | Number of forces in the force list
forceCount :: WASMSimulation -> Effect Int
forceCount = runEffectFn1 forceCount_

-- | Add a many-body (charge) force
addManyBody :: WASMSimulation -> ManyBodyConfig -> Effect ForceId
addManyBody sim cfg = runEffectFn5 addManyBody_ sim cfg.strength cfg.theta cfg.distanceMin cfg.distanceMax

-- | Add a link (spring) force over the simulation's links
addLinks :: WASMSimulation -> LinkConfig -> Effect ForceId
addLinks sim cfg = runEffectFn4 addLinks_ sim cfg.distance cfg.strength cfg.iterations

-- | Add a centering force
addCenter :: WASMSimulation -> CenterConfig -> Effect ForceId
addCenter sim cfg = runEffectFn4 addCenter_ sim cfg.x cfg.y cfg.strength

-- | Add a force pulling nodes toward an x coordinate
addForceX :: WASMSimulation -> PositionConfig -> Effect ForceId
addForceX sim cfg = runEffectFn3 addForceX_ sim cfg.target cfg.strength

-- | Add a force pulling nodes toward a y coordinate
addForceY :: WASMSimulation -> PositionConfig -> Effect ForceId
addForceY sim cfg = runEffectFn3 addForceY_ sim cfg.target cfg.strength

-- | Add a collision force
addCollide :: WASMSimulation -> CollideConfig -> Effect ForceId
addCollide sim cfg = runEffectFn4 addCollide_ sim cfg.radius cfg.strength cfg.iterations

-- | Add a radial force
addRadial :: WASMSimulation -> RadialConfig -> Effect ForceId
addRadial sim cfg = runEffectFn5 addRadial_ sim cfg.radius cfg.x cfg.y cfg.strength

-- | Give a force one strength per node (many-body, forceX, forceY, radial)
-- | or one per link (link), in node or link order. Returns false, changing
-- | nothing, for a force whose strength is a single number (center, collide).
setForceStrengths :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceStrengths = runEffectFn3 setForceStrengths_

-- | Give a force one radius per node (collide, radial). False for others.
setForceRadii :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceRadii = runEffectFn3 setForceRadii_

-- | Give a force one target coordinate per node (forceX, forceY). A NaN
-- | target leaves that node alone. False for other forces.
setForceTargets :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceTargets = runEffectFn3 setForceTargets_

-- | Give a link force one rest length per link. False for other forces.
setForceDistances :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceDistances = runEffectFn3 setForceDistances_

-- | Filter a force: one Boolean per node, false meaning the node has
-- | strength 0 under this force. For forceX, forceY and radial that leaves
-- | the node alone. For many-body it means the node exerts no charge but is
-- | still pushed by the others, which is what the D3 kernel's filtered
-- | many-body does. False for forces with no per-node strength (link,
-- | center, collide).
setForceMask :: WASMSimulation -> ForceId -> Array Boolean -> Effect Boolean
setForceMask = runEffectFn3 setForceMask_


-- =============================================================================
-- Forces d3-force does not have
-- =============================================================================
--
-- Each of these is defined in full, formula by formula, on the matching
-- `apply_*` function in the kernel's `forces.rs`, and has a plain reference
-- implementation in its `reference.rs`.

-- | Add a force pulling each node toward its own target point:
-- | v += (target - position) * strength * alpha. It does nothing until
-- | `setForceTargetsXY` gives it targets.
addTowards :: WASMSimulation -> Number -> Effect ForceId
addTowards = runEffectFn2 addTowards_

-- | Give a towards force its targets, flat: [x0, y0, x1, y1, ...] in node
-- | order. A NaN leaves that node alone. Cheap to call every tick: an array
-- | of the same length overwrites the kernel's copy in place. False for
-- | any other force.
setForceTargetsXY :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceTargetsXY = runEffectFn3 setForceTargetsXY_

-- | Add a rectangle collision force: nodes are axis-aligned rectangles,
-- | pushed apart along the axis on which a pair overlaps least.
addRectCollide
  :: WASMSimulation
  -> { halfWidth :: Number, halfHeight :: Number, strength :: Number, iterations :: Int }
  -> Effect ForceId
addRectCollide sim cfg = runEffectFn5 addRectCollide_ sim cfg.halfWidth cfg.halfHeight cfg.strength cfg.iterations

-- | Give a rectangle collision force one half-width per node.
setForceHalfWidths :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceHalfWidths = runEffectFn3 setForceHalfWidths_

-- | Give a rectangle collision force one half-height per node.
setForceHalfHeights :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceHalfHeights = runEffectFn3 setForceHalfHeights_

-- | Add a containment force, with no regions yet. `strength` steers nodes
-- | back in before they move; `hard` also clamps positions after each tick.
-- | Every node is in region 0 unless `setForceRegions` says otherwise; node
-- | radii come from `setForceRadii`.
addContainment :: WASMSimulation -> { strength :: Number, hard :: Boolean } -> Effect ForceId
addContainment sim cfg = runEffectFn3 addContainment_ sim cfg.strength cfg.hard

-- | Add a rectangular region to a containment force. Returns its index
-- | among that force's regions, or -1 if the force is not a containment.
containmentAddRect :: WASMSimulation -> ForceId -> { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number } -> Effect Int
containmentAddRect sim force r = runEffectFn6 containmentAddRect_ sim force r.x0 r.y0 r.x1 r.y1

-- | Add a circular region to a containment force. Returns its index, or -1.
containmentAddCircle :: WASMSimulation -> ForceId -> { x :: Number, y :: Number, radius :: Number } -> Effect Int
containmentAddCircle sim force c = runEffectFn5 containmentAddCircle_ sim force c.x c.y c.radius

-- | Add a convex polygon region (either winding) to a containment force.
-- | Returns its index, or -1.
containmentAddPolygon :: WASMSimulation -> ForceId -> Array { x :: Number, y :: Number } -> Effect Int
containmentAddPolygon sim force vertices =
  runEffectFn3 containmentAddPolygon_ sim force (Array.concatMap (\v -> [ v.x, v.y ]) vertices)

-- | Assign each node a region of a containment force, by index; `Nothing`
-- | leaves the node free.
setForceRegions :: WASMSimulation -> ForceId -> Array (Maybe Int) -> Effect Boolean
setForceRegions sim force regions = runEffectFn3 setForceRegions_ sim force (map orNone regions)

-- | Add a group cohesion force: each node is pulled toward the centroid of
-- | its group. Groups come from `setForceGroups`.
addCohesion :: WASMSimulation -> Number -> Effect ForceId
addCohesion = runEffectFn2 addCohesion_

-- | Give a cohesion or many-body force one group per node, as non-negative
-- | numbers; `Nothing` is "in no group".
setForceGroups :: WASMSimulation -> ForceId -> Array (Maybe Int) -> Effect Boolean
setForceGroups sim force groups = runEffectFn3 setForceGroups_ sim force (map orNone groups)

-- | Restrict a many-body force to pairs within a group, or between groups.
setManyBodyScope :: WASMSimulation -> ForceId -> ManyBodyScope -> Effect Boolean
setManyBodyScope sim force scope = runEffectFn3 setManyBodyScope_ sim force case scope of
  AllPairs -> 0
  WithinGroups -> 1
  BetweenGroups -> 2

-- | Add a directional link constraint: each link's target is pushed to be
-- | at least `gap` beyond its source along `axis` (which need not be a unit
-- | vector). Per-link gaps with `setForceGaps`, per-link strengths with
-- | `setForceStrengths`.
addDirectional
  :: WASMSimulation
  -> { gap :: Number, axis :: { x :: Number, y :: Number }, strength :: Number, iterations :: Int }
  -> Effect ForceId
addDirectional sim cfg = runEffectFn6 addDirectional_ sim cfg.gap cfg.axis.x cfg.axis.y cfg.strength cfg.iterations

-- | Give a directional constraint one gap per link.
setForceGaps :: WASMSimulation -> ForceId -> Array Number -> Effect Boolean
setForceGaps = runEffectFn3 setForceGaps_

-- | The kernel's "none" for a group or region is any negative number here.
orNone :: Maybe Int -> Int
orNone = case _ of
  Just n | n >= 0 -> n
  _ -> -1


-- =============================================================================
-- Snapshot, restore, convergence
-- =============================================================================

-- | Record everything that changes as the simulation runs. Forces and links
-- | are configuration, not state, and are not recorded.
snapshot :: WASMSimulation -> Effect Snapshot
snapshot = runEffectFn1 snapshot_

-- | Put a simulation back in a recorded state. On a simulation with the same
-- | forces and links, `restore` followed by N ticks gives exactly the
-- | positions N more ticks gave (or would have given) the original. False,
-- | changing nothing, if the snapshot is malformed.
restore :: WASMSimulation -> Snapshot -> Effect Boolean
restore = runEffectFn2 restore_

-- | Kinetic energy after the last tick, each node having unit mass: the sum
-- | of (vx^2 + vy^2) / 2. Falls toward 0 as the layout settles.
kineticEnergy :: WASMSimulation -> Effect Number
kineticEnergy = runEffectFn1 kineticEnergy_

-- | The furthest any node moved during the last tick.
maxDisplacement :: WASMSimulation -> Effect Number
maxDisplacement = runEffectFn1 maxDisplacement_

-- | The largest change in any node's step between the last two ticks: the
-- | acceleration it actually underwent. Unlike `kineticEnergy` and
-- | `maxDisplacement` it does not vanish at the far end of a swing, where a
-- | node is momentarily still. 0 until two ticks have been made.
maxAcceleration :: WASMSimulation -> Effect Number
maxAcceleration = runEffectFn1 maxAcceleration_

-- | Is the simulation at rest: over the last two ticks, did no node move
-- | further than the first number in a tick, and no node's step change by
-- | more than the second? False until two ticks have been made.
settled :: WASMSimulation -> Number -> Number -> Effect Boolean
settled = runEffectFn3 settled_

-- | Make a collide force's strength follow alpha (true) or be d3's (false,
-- | the default). False for any other force.
setCollideAlphaScaled :: WASMSimulation -> ForceId -> Boolean -> Effect Boolean
setCollideAlphaScaled = runEffectFn3 setCollideAlphaScaled_

-- | Filter a collide force by node: a node given false collides with
-- | nothing under this force. False for any other force.
setCollideFilter :: WASMSimulation -> ForceId -> Array Boolean -> Effect Boolean
setCollideFilter = runEffectFn3 setCollideFilter_


-- =============================================================================
-- Simulation Control
-- =============================================================================

-- | Set alpha (temperature). Higher = more movement.
setAlpha :: WASMSimulation -> Number -> Effect Unit
setAlpha = setAlpha_

-- | Get current alpha
getAlpha :: WASMSimulation -> Effect Number
getAlpha = getAlpha_

-- | Set alpha decay rate
setAlphaDecay :: WASMSimulation -> Number -> Effect Unit
setAlphaDecay = setAlphaDecay_

-- | Set the share of its velocity a node keeps from one tick to the next:
-- | 0.6 keeps 60% and loses 40% (d3's default, which d3 writes as a
-- | `velocityDecay` of 0.4).
setVelocityRetention :: WASMSimulation -> Number -> Effect Unit
setVelocityRetention = setVelocityRetention_

-- | Deprecated: the old name of `setVelocityRetention`, kept so that
-- | existing code compiles. It does exactly what it always did: the number
-- | is the share of velocity KEPT each tick, not the share lost.
setVelocityDecay :: WASMSimulation -> Number -> Effect Unit
setVelocityDecay = setVelocityRetention_

-- | Set the alpha below which the simulation counts as finished
setAlphaMin :: WASMSimulation -> Number -> Effect Unit
setAlphaMin = runEffectFn2 setAlphaMin_

-- | Set the alpha the simulation cools toward
setAlphaTarget :: WASMSimulation -> Number -> Effect Unit
setAlphaTarget = runEffectFn2 setAlphaTarget_

-- | Rewind the stream of nudges the kernel uses to separate coincident
-- | nodes. The kernel is deterministic: the stream is d3's own seeded
-- | generator, one per simulation, so a simulation rebuilt the same way
-- | repeats exactly. This puts an existing simulation back at the start.
resetRandom :: WASMSimulation -> Effect Unit
resetRandom = runEffectFn1 resetRandom_

-- | Reheat simulation (reset alpha to 1.0)
reheat :: WASMSimulation -> Effect Unit
reheat = reheat_

-- | Run a single simulation tick. Returns new alpha value.
tick :: WASMSimulation -> Effect Number
tick = tick_

-- | Run multiple ticks. Stops early if alpha < alphaMin.
tickN :: WASMSimulation -> Int -> Effect Number
tickN = tickN_

-- | Check if simulation is still running (alpha >= alphaMin)
isRunning :: WASMSimulation -> Effect Boolean
isRunning = isRunning_


-- =============================================================================
-- Queries
-- =============================================================================

-- | Get number of nodes
nodeCount :: WASMSimulation -> Effect Int
nodeCount = nodeCount_

-- | Get number of links
linkCount :: WASMSimulation -> Effect Int
linkCount = linkCount_


-- =============================================================================
-- Node Array Sync
-- =============================================================================

-- | Copy positions from PureScript node array to WASM
syncPositionsToWasm :: forall r. WASMSimulation -> Array { x :: Number, y :: Number | r } -> Effect Unit
syncPositionsToWasm = syncPositionsToWasm_

-- | Copy positions from WASM back to PureScript node array.
-- | Warning: This mutates the input array!
syncPositionsFromWasm :: forall r. WASMSimulation -> Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r } -> Effect (Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r })
syncPositionsFromWasm = syncPositionsFromWasm_

-- | Copy position, velocity and pinning (`fx`/`fy`, per axis) from
-- | PureScript nodes to WASM. A node whose position is NaN is placed on
-- | d3's phyllotaxis spiral.
syncNodesToWasm
  :: forall r
   . WASMSimulation
  -> Array { x :: Number, y :: Number, vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | r }
  -> Effect Unit
syncNodesToWasm = runEffectFn2 syncNodesToWasm_

-- | Copy the pins alone (fx/fy, null = free) from PureScript nodes to WASM.
syncPinsToWasm
  :: forall r
   . WASMSimulation
  -> Array { fx :: Nullable Number, fy :: Nullable Number | r }
  -> Effect Unit
syncPinsToWasm = runEffectFn2 syncPinsToWasm_

-- | The nodes with the pins a snapshot recorded, as new records. The
-- | snapshot must be of as many nodes.
withPinsFromSnapshot
  :: forall r
   . Snapshot
  -> Array { fx :: Nullable Number, fy :: Nullable Number | r }
  -> Array { fx :: Nullable Number, fy :: Nullable Number | r }
withPinsFromSnapshot = runFn2 withPinsFromSnapshot_

-- | Copy position and velocity from WASM back to PureScript nodes.
-- | Warning: This mutates the input array!
syncNodesFromWasm
  :: forall r
   . WASMSimulation
  -> Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r }
  -> Effect (Array { x :: Number, y :: Number, vx :: Number, vy :: Number | r })
syncNodesFromWasm = runEffectFn2 syncNodesFromWasm_
