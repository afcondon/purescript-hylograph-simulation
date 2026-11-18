-- | Declarative Force Setup
-- |
-- | Unified system for declaratively configuring force simulations.
-- | Replaces the older ForceSpec (too simple) and Config.* modules (wrong abstraction layer).
-- |
-- | Key features:
-- | - Static or dynamic (per-node, or per-link) parameter values
-- | - Optional filters to apply forces selectively
-- | - Idempotent `applySetup` that works for both initial setup and updates
-- |
-- | Example:
-- | ```purescript
-- | mySetup = setup "physics"
-- |   [ manyBody "charge" # withStrength (static (-30.0))
-- |   , collide "collision" # withRadius (dynamic _.r)
-- |   , positionX "gridX" # withX (dynamic _.gridX) # withStrength (static 0.1)
-- |   , positionY "gridY" # withY (dynamic _.gridY) # withStrength (static 0.1)
-- |   ]
-- |
-- | -- Apply to simulation (idempotent - can call repeatedly)
-- | applySetup mySetup sim
-- |
-- | -- Update: just call applySetup with new config
-- | applySetup (mySetup # removeForce "charge") sim
-- | ```
-- |
-- | A force is one constructor of `ForceConfig`, carrying a record of exactly
-- | the parameters that force has. The smart constructors (`manyBody`,
-- | `collide`, ...) and the `with...` builders are the intended way to make
-- | one; a builder that does not apply to a force leaves it unchanged.
-- |
-- | Seven forces are d3-force's and run on both engines. Five are not in
-- | d3-force (`towards`, `rectCollide`, `contain`, `cohesion`, `directional`),
-- | nor is a many-body force scoped by group; those run on the WASM engine
-- | only, and `applySetup` here (the D3 engine) throws on a setup that
-- | contains one rather than quietly dropping it.
module Hylograph.ForceEngine.Setup
  ( -- * Value Types (static or dynamic)
    Value(..)
  , static
  , dynamic
  , valueToNumber
  , LinkEnds
  , LinkStrength(..)

    -- * Force Configuration
  , ForceType(..)  -- Export constructors for engine implementations
  , ForceConfig(..)
  , ManyBodyConfig
  , ManyBodyScope(..)
  , CollideConfig
  , LinkConfig
  , CenterConfig
  , PositionConfig
  , RadialConfig
  , TowardsConfig
  , RectCollideConfig
  , ContainConfig
  , Region(..)
  , CohesionConfig
  , DirectionalConfig
  , forceName
  , forceType
  , manyBody
  , collide
  , link
  , center
  , positionX
  , positionY
  , radial
  , towards
  , rectCollide
  , contain
  , containEach
  , cohesion
  , directional

    -- * Force Modifiers
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
  , withLinkDistance
  , withLinkStrength
  , withDegreeStrength
  , withTarget
  , withAlphaScaling
  , withHalfWidth
  , withHalfHeight
  , withRegions
  , withRegion
  , withHardLimit
  , withGroup
  , withinGroups
  , betweenGroups
  , withGap
  , withAxis

    -- * Evaluating link values (for engine implementations)
  , resolveLinks
  , linkValues
  , linkStrengths
  , d3EngineSupports

    -- * Setup Type
  , Setup
  , SetupParams
  , defaultParams
  , setup
  , setupWithParams
  , getForces
  , getParams

    -- * Setup Modifiers
  , addForce
  , removeForce
  , updateForce
  , withAlpha
  , withAlphaDecay
  , withAlphaTarget
  , withAlphaMin
  , withVelocityRetention
  , withVelocityDecay

    -- * Apply to Simulation (the key function)
  , applySetup

    -- * Apply with Data (GUP semantics)
  , GUPResult
  , GUPLinkResult
  , applySetupWithData

    -- * GUP Helpers (for engine implementations)
  , NodeGUPInternal
  , computeNodeGUP
  , computeLinkGUP
  , mergeSimulationState

    -- * Incremental Operations
  , addForceToSim
  , removeForceFromSim
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Hylograph.ForceEngine.Core as Core
import Hylograph.ForceEngine.Simulation (Simulation, SimulationNode, NodeID)

-- =============================================================================
-- Value Type (static, or computed per node / per link)
-- =============================================================================

-- | A value that is one constant, or computed from each element it applies
-- | to: `Value node a` per node, `Value (LinkEnds node) a` per link.
data Value node a
  = Static a
  | Dynamic (node -> a)

-- | Create a static value
static :: forall node a. a -> Value node a
static = Static

-- | Create a dynamic (per-node, or per-link) value
dynamic :: forall node a. (node -> a) -> Value node a
dynamic = Dynamic

-- | Extract a number from a Value (for static values only, dynamic returns default)
-- | Used internally; prefer the accessor pattern for dynamic values
valueToNumber :: forall node. Value node Number -> Number
valueToNumber = case _ of
  Static n -> n
  Dynamic _ -> 0.0 -- Placeholder, actual value comes from accessor

-- | What a per-link value is computed from: the nodes at the link's two ends
-- | and the link's position in the simulation's link array.
type LinkEnds node = { source :: node, target :: node, index :: Int }

-- | How strong each link's spring is.
data LinkStrength node
  -- | A constant, or computed per link.
  = LinkStrength (Value (LinkEnds node) Number)
  -- | d3-force's own default: 1 / min(links at the source, links at the
  -- | target), so that heavily linked nodes are not pulled apart.
  | ByDegree

-- =============================================================================
-- Force Configuration
-- =============================================================================

-- | Configuration for a single force: one constructor per kind of force,
-- | each with a record of that force's own parameters.
-- | Parameterized by node type for dynamic values and filters.
data ForceConfig node
  = ManyBodyForce (ManyBodyConfig node)
  | CollideForce (CollideConfig node)
  | LinkForce (LinkConfig node)
  | CenterForce CenterConfig
  | PositionXForce (PositionConfig node)
  | PositionYForce (PositionConfig node)
  | RadialForce (RadialConfig node)
  | TowardsForce (TowardsConfig node)
  | RectCollideForce (RectCollideConfig node)
  | ContainForce (ContainConfig node)
  | CohesionForce (CohesionConfig node)
  | DirectionalForce (DirectionalConfig node)

-- | Charge: every node repels (negative) or attracts (positive) every other.
type ManyBodyConfig node =
  { name :: String
  , strength :: Value node Number
  , theta :: Number        -- Barnes-Hut approximation
  , distanceMin :: Number
  , distanceMax :: Number
  , filter :: Maybe (node -> Boolean)  -- a filtered-out node exerts nothing (it is still pushed)
  , scope :: ManyBodyScope node
  }

-- | Which pairs of nodes a many-body force acts between. A node whose group
-- | is `Nothing` is in no group: it has no same-group pairs, and is in a
-- | different group from everyone. Scopes other than `AllPairs` need the
-- | WASM engine.
data ManyBodyScope node
  = AllPairs
  | WithinGroups (node -> Maybe Int)
  | BetweenGroups (node -> Maybe Int)

-- | Circles that must not overlap.
-- |
-- | `filter` and `alphaScaled` need the WASM engine. A node the filter leaves
-- | out collides with nothing. With `alphaScaled` the strength used is
-- | strength * alpha, so the collision weakens as the simulation cools and
-- | gives way to forces that hold a node at a place; without it (d3's
-- | collide) a collision is as hard at rest as in flight.
type CollideConfig node =
  { name :: String
  , radius :: Value node Number
  , strength :: Number
  , iterations :: Int
  , filter :: Maybe (node -> Boolean)
  , alphaScaled :: Boolean
  }

-- | Springs along the simulation's links.
type LinkConfig node =
  { name :: String
  , distance :: Value (LinkEnds node) Number
  , strength :: LinkStrength node
  , iterations :: Int
  }

-- | Move the mean position of all nodes toward a point.
type CenterConfig =
  { name :: String
  , x :: Number
  , y :: Number
  , strength :: Number
  }

-- | Pull toward an x (PositionXForce) or a y (PositionYForce).
type PositionConfig node =
  { name :: String
  , target :: Value node Number
  , strength :: Value node Number
  , filter :: Maybe (node -> Boolean)
  }

-- | Pull toward a circle of a given radius around (x, y).
type RadialConfig node =
  { name :: String
  , radius :: Value node Number
  , x :: Number
  , y :: Number
  , strength :: Value node Number
  , filter :: Maybe (node -> Boolean)
  }

-- | WASM engine only. A spring from each node to its own target point:
-- | v += (target - position) * strength * alpha. `Nothing` leaves a node
-- | alone. For morphing between layouts: the targets can be replaced
-- | between ticks without re-applying the setup
-- | (`Hylograph.ForceEngine.Setup.WASM.retarget`).
type TowardsConfig node =
  { name :: String
  , target :: node -> Maybe { x :: Number, y :: Number }
  , strength :: Value node Number
  , filter :: Maybe (node -> Boolean)
  }

-- | WASM engine only. Axis-aligned rectangles (for labels) that must not
-- | overlap: an overlapping pair is pushed apart along the axis on which it
-- | overlaps least, the larger rectangle moving less.
type RectCollideConfig node =
  { name :: String
  , halfWidth :: Value node Number
  , halfHeight :: Value node Number
  , strength :: Number
  , iterations :: Int
  }

-- | WASM engine only. Keep nodes inside regions. Each node is confined to
-- | `regions !! i` where `region node == Just i` (`Nothing`: free), staying
-- | `radius` inside the boundary. `strength` (0 to 1) steers a node that is
-- | about to leave back toward the nearest allowed point; `hard` also clamps
-- | positions into the region after each tick, so that no node is ever
-- | drawn outside.
type ContainConfig node =
  { name :: String
  , regions :: Array Region
  , region :: node -> Maybe Int
  , radius :: Value node Number
  , strength :: Number
  , hard :: Boolean
  }

-- | A region nodes can be confined to.
data Region
  = RectRegion { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
  | CircleRegion { x :: Number, y :: Number, radius :: Number }
  -- | Convex, vertices in either winding.
  | PolygonRegion (Array { x :: Number, y :: Number })

-- | WASM engine only. Pull each node toward the centroid of its group:
-- | v += (centroid - position) * strength * alpha. `Nothing`: in no group.
type CohesionConfig node =
  { name :: String
  , group :: node -> Maybe Int
  , strength :: Value node Number
  , filter :: Maybe (node -> Boolean)
  }

-- | WASM engine only. Over the simulation's links: push each link's target
-- | to be at least `gap` beyond its source along `axis` (children below
-- | parents, with axis (0, 1)). Links already that far apart are left alone.
type DirectionalConfig node =
  { name :: String
  , gap :: Value (LinkEnds node) Number
  , axis :: { x :: Number, y :: Number }
  , strength :: Value (LinkEnds node) Number
  , iterations :: Int
  }

-- | Force type enumeration
data ForceType
  = ForceManyBody
  | ForceCollide
  | ForceLink
  | ForceCenter
  | ForcePositionX
  | ForcePositionY
  | ForceRadial
  | ForceTowards
  | ForceRectCollide
  | ForceContain
  | ForceCohesion
  | ForceDirectional

derive instance eqForceType :: Eq ForceType

-- | Get the name of a force config
forceName :: forall node. ForceConfig node -> String
forceName = case _ of
  ManyBodyForce c -> c.name
  CollideForce c -> c.name
  LinkForce c -> c.name
  CenterForce c -> c.name
  PositionXForce c -> c.name
  PositionYForce c -> c.name
  RadialForce c -> c.name
  TowardsForce c -> c.name
  RectCollideForce c -> c.name
  ContainForce c -> c.name
  CohesionForce c -> c.name
  DirectionalForce c -> c.name

-- | Which kind of force a config is
forceType :: forall node. ForceConfig node -> ForceType
forceType = case _ of
  ManyBodyForce _ -> ForceManyBody
  CollideForce _ -> ForceCollide
  LinkForce _ -> ForceLink
  CenterForce _ -> ForceCenter
  PositionXForce _ -> ForcePositionX
  PositionYForce _ -> ForcePositionY
  RadialForce _ -> ForceRadial
  TowardsForce _ -> ForceTowards
  RectCollideForce _ -> ForceRectCollide
  ContainForce _ -> ForceContain
  CohesionForce _ -> ForceCohesion
  DirectionalForce _ -> ForceDirectional

-- =============================================================================
-- Force Constructors (smart constructors with defaults)
-- =============================================================================

-- | Create a many-body (charge) force
-- | Default: repulsive with strength -30
manyBody :: forall node. String -> ForceConfig node
manyBody name = ManyBodyForce
  { name
  , strength: Static (-30.0)
  , theta: 0.9
  , distanceMin: 1.0
  , distanceMax: 1.0e10
  , filter: Nothing
  , scope: AllPairs
  }

-- | Create a collision force
-- | Default: radius 1, strength 1
collide :: forall node. String -> ForceConfig node
collide name = CollideForce
  { name, radius: Static 1.0, strength: 1.0, iterations: 1, filter: Nothing, alphaScaled: false }

-- | Create a link (spring) force
-- | Default: distance 30, strength 1
link :: forall node. String -> ForceConfig node
link name = LinkForce { name, distance: Static 30.0, strength: LinkStrength (Static 1.0), iterations: 1 }

-- | Create a centering force
-- | Default: center at (0, 0), strength 1
center :: forall node. String -> ForceConfig node
center name = CenterForce { name, x: 0.0, y: 0.0, strength: 1.0 }

-- | Create an X-positioning force
-- | Default: x = 0, strength 0.1
positionX :: forall node. String -> ForceConfig node
positionX name = PositionXForce { name, target: Static 0.0, strength: Static 0.1, filter: Nothing }

-- | Create a Y-positioning force
-- | Default: y = 0, strength 0.1
positionY :: forall node. String -> ForceConfig node
positionY name = PositionYForce { name, target: Static 0.0, strength: Static 0.1, filter: Nothing }

-- | Create a radial force
-- | Default: radius 100, center (0,0), strength 0.1
radial :: forall node. String -> ForceConfig node
radial name = RadialForce { name, radius: Static 100.0, x: 0.0, y: 0.0, strength: Static 0.1, filter: Nothing }

-- | Create a force pulling each node toward its own point (WASM engine).
-- | Default: strength 0.1. A node whose target is `Nothing` is left alone.
towards :: forall node. String -> (node -> Maybe { x :: Number, y :: Number }) -> ForceConfig node
towards name target = TowardsForce { name, target, strength: Static 0.1, filter: Nothing }

-- | Create a rectangle collision force (WASM engine).
-- | Default: half-width 1, half-height 1, strength 1, 1 iteration.
rectCollide :: forall node. String -> ForceConfig node
rectCollide name = RectCollideForce
  { name, halfWidth: Static 1.0, halfHeight: Static 1.0, strength: 1.0, iterations: 1 }

-- | Create a containment force keeping every node in one region (WASM
-- | engine). Default: radius 0, strength 1, with the hard limit on. For
-- | several regions use `withRegions` and `withRegion`.
contain :: forall node. String -> Region -> ForceConfig node
contain name region = ContainForce
  { name, regions: [ region ], region: const (Just 0), radius: Static 0.0, strength: 1.0, hard: true }

-- | Create a containment force keeping each node in a region of its own
-- | (WASM engine): node `n` in `regions !! i` where `region n == Just i`, and
-- | free where it is `Nothing`. Defaults as `contain`.
containEach :: forall node. String -> Array Region -> (node -> Maybe Int) -> ForceConfig node
containEach name regions region = ContainForce
  { name, regions, region, radius: Static 0.0, strength: 1.0, hard: true }

-- | Create a group cohesion force (WASM engine). Default: strength 0.1.
cohesion :: forall node. String -> (node -> Maybe Int) -> ForceConfig node
cohesion name group = CohesionForce { name, group, strength: Static 0.1, filter: Nothing }

-- | Create a directional link constraint (WASM engine).
-- | Default: gap 30 along (0, 1) (targets below sources), strength 1.
directional :: forall node. String -> ForceConfig node
directional name = DirectionalForce
  { name, gap: Static 30.0, axis: { x: 0.0, y: 1.0 }, strength: Static 1.0, iterations: 1 }

-- =============================================================================
-- Force Modifiers (fluent API)
-- =============================================================================
--
-- A modifier that does not apply to a force returns it unchanged.

-- | Set strength.
-- |
-- | Per node (`dynamic`) for ManyBody, PositionX/Y, Radial, Towards and
-- | Cohesion. Collide, Center, RectCollide and Contain have one strength: a
-- | `static` sets it, a `dynamic` is ignored. For Link and Directional a
-- | `static` sets every link's strength and a `dynamic`, being a function of
-- | a node, is ignored: use `withLinkStrength`.
withStrength :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withStrength v = case _ of
  ManyBodyForce c -> ManyBodyForce c { strength = v }
  PositionXForce c -> PositionXForce c { strength = v }
  PositionYForce c -> PositionYForce c { strength = v }
  RadialForce c -> RadialForce c { strength = v }
  TowardsForce c -> TowardsForce c { strength = v }
  CohesionForce c -> CohesionForce c { strength = v }
  CollideForce c -> CollideForce c { strength = staticOr c.strength v }
  CenterForce c -> CenterForce c { strength = staticOr c.strength v }
  RectCollideForce c -> RectCollideForce c { strength = staticOr c.strength v }
  ContainForce c -> ContainForce c { strength = staticOr c.strength v }
  LinkForce c -> case v of
    Static s -> LinkForce c { strength = LinkStrength (Static s) }
    Dynamic _ -> LinkForce c
  DirectionalForce c -> case v of
    Static s -> DirectionalForce c { strength = Static s }
    Dynamic _ -> DirectionalForce c

-- | The number in a static value, or a fallback for a dynamic one.
staticOr :: forall node. Number -> Value node Number -> Number
staticOr fallback = case _ of
  Static n -> n
  Dynamic _ -> fallback

-- | Set radius (Collide and Radial; the node's own radius for Contain)
withRadius :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withRadius v = case _ of
  CollideForce c -> CollideForce c { radius = v }
  RadialForce c -> RadialForce c { radius = v }
  ContainForce c -> ContainForce c { radius = v }
  other -> other

-- | Set X: the target of PositionX (per node if `dynamic`), or the centre of
-- | Center and Radial (`static` only).
withX :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withX v = case _ of
  PositionXForce c -> PositionXForce c { target = v }
  CenterForce c -> CenterForce c { x = staticOr c.x v }
  RadialForce c -> RadialForce c { x = staticOr c.x v }
  other -> other

-- | Set Y: the target of PositionY (per node if `dynamic`), or the centre of
-- | Center and Radial (`static` only).
withY :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withY v = case _ of
  PositionYForce c -> PositionYForce c { target = v }
  CenterForce c -> CenterForce c { y = staticOr c.y v }
  RadialForce c -> RadialForce c { y = staticOr c.y v }
  other -> other

-- | Set every link's distance (for Link; `static` only: a `dynamic` here is
-- | a function of a node and is ignored, use `withLinkDistance`).
withDistance :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withDistance v = case _ of
  LinkForce c -> case v of
    Static d -> LinkForce c { distance = Static d }
    Dynamic _ -> LinkForce c
  other -> other

-- | Set link distance per link, from the nodes at its two ends (for Link)
withLinkDistance :: forall node. Value (LinkEnds node) Number -> ForceConfig node -> ForceConfig node
withLinkDistance v = case _ of
  LinkForce c -> LinkForce c { distance = v }
  other -> other

-- | Set strength per link, from the nodes at its two ends (Link, Directional)
withLinkStrength :: forall node. Value (LinkEnds node) Number -> ForceConfig node -> ForceConfig node
withLinkStrength v = case _ of
  LinkForce c -> LinkForce c { strength = LinkStrength v }
  DirectionalForce c -> DirectionalForce c { strength = v }
  other -> other

-- | Give links d3-force's default strength, 1 / min(links at the source,
-- | links at the target) (for Link)
withDegreeStrength :: forall node. ForceConfig node -> ForceConfig node
withDegreeStrength = case _ of
  LinkForce c -> LinkForce c { strength = ByDegree }
  other -> other

-- | Set theta (Barnes-Hut approximation, for ManyBody)
withTheta :: forall node. Number -> ForceConfig node -> ForceConfig node
withTheta v = case _ of
  ManyBodyForce c -> ManyBodyForce c { theta = v }
  other -> other

-- | Set iterations (for Collide, Link, RectCollide, Directional)
withIterations :: forall node. Int -> ForceConfig node -> ForceConfig node
withIterations v = case _ of
  CollideForce c -> CollideForce c { iterations = v }
  LinkForce c -> LinkForce c { iterations = v }
  RectCollideForce c -> RectCollideForce c { iterations = v }
  DirectionalForce c -> DirectionalForce c { iterations = v }
  other -> other

-- | Set distance min (for ManyBody)
withDistanceMin :: forall node. Number -> ForceConfig node -> ForceConfig node
withDistanceMin v = case _ of
  ManyBodyForce c -> ManyBodyForce c { distanceMin = v }
  other -> other

-- | Set distance max (for ManyBody)
withDistanceMax :: forall node. Number -> ForceConfig node -> ForceConfig node
withDistanceMax v = case _ of
  ManyBodyForce c -> ManyBodyForce c { distanceMax = v }
  other -> other

-- | Add a filter predicate: the force only applies to matching nodes.
-- | For ManyBody (a filtered-out node exerts no charge but is still pushed),
-- | Collide (a filtered-out node collides with nothing; WASM engine),
-- | PositionX, PositionY, Radial, Towards and Cohesion.
withFilter :: forall node. (node -> Boolean) -> ForceConfig node -> ForceConfig node
withFilter pred = case _ of
  ManyBodyForce c -> ManyBodyForce c { filter = Just pred }
  CollideForce c -> CollideForce c { filter = Just pred }
  PositionXForce c -> PositionXForce c { filter = Just pred }
  PositionYForce c -> PositionYForce c { filter = Just pred }
  RadialForce c -> RadialForce c { filter = Just pred }
  TowardsForce c -> TowardsForce c { filter = Just pred }
  CohesionForce c -> CohesionForce c { filter = Just pred }
  other -> other

-- | Make a collision's strength follow alpha, so that it yields as the
-- | simulation cools (for Collide; WASM engine). Off by default.
withAlphaScaling :: forall node. Boolean -> ForceConfig node -> ForceConfig node
withAlphaScaling alphaScaled = case _ of
  CollideForce c -> CollideForce c { alphaScaled = alphaScaled }
  other -> other

-- | Set each node's target point (for Towards)
withTarget :: forall node. (node -> Maybe { x :: Number, y :: Number }) -> ForceConfig node -> ForceConfig node
withTarget target = case _ of
  TowardsForce c -> TowardsForce c { target = target }
  other -> other

-- | Set the rectangles' half-width (for RectCollide)
withHalfWidth :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withHalfWidth v = case _ of
  RectCollideForce c -> RectCollideForce c { halfWidth = v }
  other -> other

-- | Set the rectangles' half-height (for RectCollide)
withHalfHeight :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withHalfHeight v = case _ of
  RectCollideForce c -> RectCollideForce c { halfHeight = v }
  other -> other

-- | Set the regions (for Contain); `withRegion` says which node is in which
withRegions :: forall node. Array Region -> ForceConfig node -> ForceConfig node
withRegions regions = case _ of
  ContainForce c -> ContainForce c { regions = regions }
  other -> other

-- | Assign each node a region by its index in the regions array, or
-- | `Nothing` to leave it free (for Contain)
withRegion :: forall node. (node -> Maybe Int) -> ForceConfig node -> ForceConfig node
withRegion region = case _ of
  ContainForce c -> ContainForce c { region = region }
  other -> other

-- | Turn the clamp after each tick on or off (for Contain). Off, the region
-- | is a soft boundary that nodes can be pushed across for a while.
withHardLimit :: forall node. Boolean -> ForceConfig node -> ForceConfig node
withHardLimit hard = case _ of
  ContainForce c -> ContainForce c { hard = hard }
  other -> other

-- | Set each node's group (for Cohesion)
withGroup :: forall node. (node -> Maybe Int) -> ForceConfig node -> ForceConfig node
withGroup group = case _ of
  CohesionForce c -> CohesionForce c { group = group }
  other -> other

-- | Restrict a ManyBody force to pairs of nodes in the same group
withinGroups :: forall node. (node -> Maybe Int) -> ForceConfig node -> ForceConfig node
withinGroups group = case _ of
  ManyBodyForce c -> ManyBodyForce c { scope = WithinGroups group }
  other -> other

-- | Restrict a ManyBody force to pairs of nodes in different groups
betweenGroups :: forall node. (node -> Maybe Int) -> ForceConfig node -> ForceConfig node
betweenGroups group = case _ of
  ManyBodyForce c -> ManyBodyForce c { scope = BetweenGroups group }
  other -> other

-- | Set the gap each link's target must be beyond its source, per link if
-- | `dynamic` (for Directional)
withGap :: forall node. Value (LinkEnds node) Number -> ForceConfig node -> ForceConfig node
withGap v = case _ of
  DirectionalForce c -> DirectionalForce c { gap = v }
  other -> other

-- | Set the direction targets are pushed in; it need not be a unit vector
-- | (for Directional)
withAxis :: forall node. { x :: Number, y :: Number } -> ForceConfig node -> ForceConfig node
withAxis axis = case _ of
  DirectionalForce c -> DirectionalForce c { axis = axis }
  other -> other

-- =============================================================================
-- Evaluating link values
-- =============================================================================

-- | Pair each link with the nodes at its ends. Links name nodes by `id`. A
-- | link naming a node that is not there is left out; `index` counts the
-- | links that are kept, which is the order an engine holds them in.
resolveLinks
  :: forall row linkRow
   . Array (SimulationNode row)
  -> Array { source :: NodeID, target :: NodeID | linkRow }
  -> Array (LinkEnds (SimulationNode row))
resolveLinks nodes links =
  Array.mapWithIndex (\index ends -> { source: ends.source, target: ends.target, index })
    $ Array.mapMaybe endsOf links
  where
  byId = Map.fromFoldable $ map (\node -> Tuple node.id node) nodes
  endsOf l = ado
    source <- Map.lookup l.source byId
    target <- Map.lookup l.target byId
    in { source, target }

-- | One number per link.
linkValues :: forall node. Value (LinkEnds node) Number -> Array (LinkEnds node) -> Array Number
linkValues value ends = case value of
  Static n -> map (const n) ends
  Dynamic f -> map f ends

-- | One strength per link. `ByDegree` is d3-force's default,
-- | 1 / min(links at the source, links at the target).
linkStrengths
  :: forall row
   . LinkStrength (SimulationNode row)
  -> Array (LinkEnds (SimulationNode row))
  -> Array Number
linkStrengths strength ends = case strength of
  LinkStrength value -> linkValues value ends
  ByDegree -> map byDegree ends
  where
  count = Map.fromFoldableWith (+) $ Array.concatMap (\e -> [ Tuple e.source.id 1, Tuple e.target.id 1 ]) ends
  degree id = fromMaybe 1 (Map.lookup id count)
  byDegree e = 1.0 / toNumber (min (degree e.source.id) (degree e.target.id))

-- =============================================================================
-- Setup Type
-- =============================================================================

-- | Simulation parameters
type SetupParams =
  { alpha :: Number
  , alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  -- | The share of its velocity a node keeps from one tick to the next
  -- | (`v *= velocityRetention`). This is 1 minus what d3 calls
  -- | `velocityDecay`. It was called `velocityDecay` here until 2026-11-18,
  -- | with this same meaning.
  , velocityRetention :: Number
  }

-- | Default simulation parameters
defaultParams :: SetupParams
defaultParams =
  { alpha: 1.0
  , alphaMin: 0.001
  , alphaDecay: 0.01
  , alphaTarget: 0.0
  , velocityRetention: 0.4
  }

-- | Complete setup: forces + simulation params
type Setup node =
  { name :: String
  , forces :: Array (ForceConfig node)
  , params :: SetupParams
  }

-- | Create a setup with default params
setup :: forall node. String -> Array (ForceConfig node) -> Setup node
setup name forces =
  { name
  , forces
  , params: defaultParams
  }

-- | Create a setup with custom params
setupWithParams :: forall node. String -> Array (ForceConfig node) -> SetupParams -> Setup node
setupWithParams name forces params =
  { name
  , forces
  , params
  }

-- | Get forces from setup
getForces :: forall node. Setup node -> Array (ForceConfig node)
getForces s = s.forces

-- | Get params from setup
getParams :: forall node. Setup node -> SetupParams
getParams s = s.params

-- =============================================================================
-- Setup Modifiers
-- =============================================================================

-- | Add a force to the setup
addForce :: forall node. ForceConfig node -> Setup node -> Setup node
addForce fc s = s { forces = Array.snoc s.forces fc }

-- | Remove a force by name
removeForce :: forall node. String -> Setup node -> Setup node
removeForce name s = s { forces = Array.filter (\fc -> forceName fc /= name) s.forces }

-- | Update a force by name (replace if exists)
updateForce :: forall node. ForceConfig node -> Setup node -> Setup node
updateForce fc s = s { forces = map replace s.forces }
  where
  replace existing = if forceName existing == forceName fc then fc else existing

-- | Set alpha
withAlpha :: forall node. Number -> Setup node -> Setup node
withAlpha v s = s { params = s.params { alpha = v } }

-- | Set alpha decay
withAlphaDecay :: forall node. Number -> Setup node -> Setup node
withAlphaDecay v s = s { params = s.params { alphaDecay = v } }

-- | Set alpha target
withAlphaTarget :: forall node. Number -> Setup node -> Setup node
withAlphaTarget v s = s { params = s.params { alphaTarget = v } }

-- | Set alpha min
withAlphaMin :: forall node. Number -> Setup node -> Setup node
withAlphaMin v s = s { params = s.params { alphaMin = v } }

-- | Set the share of its velocity a node keeps from one tick to the next
-- | (0.4 by default). 1 loses nothing; 0 stops every node dead each tick.
withVelocityRetention :: forall node. Number -> Setup node -> Setup node
withVelocityRetention v s = s { params = s.params { velocityRetention = v } }

-- | Deprecated: the old name of `withVelocityRetention`, kept so that
-- | existing code compiles. It does exactly what it always did: the number
-- | is the share of velocity KEPT each tick, not the share lost, which is
-- | the opposite of what d3 means by `velocityDecay`.
withVelocityDecay :: forall node. Number -> Setup node -> Setup node
withVelocityDecay = withVelocityRetention

-- =============================================================================
-- Apply Setup to Simulation (the key function)
-- =============================================================================

-- | Can the D3 engine run this force? It has d3-force's seven forces and
-- | nothing else: no Towards, RectCollide, Contain, Cohesion or Directional,
-- | no ManyBody scoped by group, and no Collide with a filter or scaled by
-- | alpha.
d3EngineSupports :: forall node. ForceConfig node -> Boolean
d3EngineSupports = case _ of
  ManyBodyForce c -> case c.scope of
    AllPairs -> true
    WithinGroups _ -> false
    BetweenGroups _ -> false
  CollideForce c -> isNothing c.filter && not c.alphaScaled
  LinkForce _ -> true
  CenterForce _ -> true
  PositionXForce _ -> true
  PositionYForce _ -> true
  RadialForce _ -> true
  TowardsForce _ -> false
  RectCollideForce _ -> false
  ContainForce _ -> false
  CohesionForce _ -> false
  DirectionalForce _ -> false

-- | Throw if the D3 engine cannot run a force, naming it.
requireD3Support :: forall node. ForceConfig node -> Effect Unit
requireD3Support fc = unless (d3EngineSupports fc) do
  throw $ "Hylograph.ForceEngine.Setup: the D3 engine cannot run the force \""
    <> forceName fc
    <> "\". Towards, RectCollide, Contain, Cohesion, Directional, group-scoped ManyBody "
    <> "and a Collide with a filter or scaled by alpha exist only in the WASM engine (Hylograph.ForceEngine.Setup.WASM). "
    <> "Nothing was applied."

-- | Apply a setup to a simulation.
-- |
-- | This is idempotent - calling with the same setup does minimal work:
-- | - Forces in setup but not simulation: create and add
-- | - Forces in simulation but not setup: remove
-- |
-- | After applying, forces are re-initialized with current nodes.
-- |
-- | Throws, before changing anything, if the setup contains a force the D3
-- | engine does not have (see `d3EngineSupports`).
-- |
-- | Example:
-- | ```purescript
-- | -- Initial setup
-- | applySetup mySetup sim
-- |
-- | -- Later: update setup (just call again)
-- | applySetup (mySetup # removeForce "charge") sim
-- | ```
applySetup
  :: forall row linkRow
   . Setup (SimulationNode row)
  -> Simulation row linkRow
  -> Effect Unit
applySetup setupConfig sim = do
  for_ setupConfig.forces requireD3Support

  nodes <- Ref.read sim.nodes
  links <- Ref.read sim.links
  currentForces <- Ref.read sim.forces

  -- Compute which forces to remove (forces in sim but not in setup)
  let desiredNames = Set.fromFoldable $ map forceName setupConfig.forces
  let currentNames = Set.fromFoldable $ Map.keys currentForces
  let toRemove = Set.difference currentNames desiredNames

  -- Remove forces no longer in setup
  for_ (Set.toUnfoldable toRemove :: Array String) \name -> do
    Ref.modify_ (Map.delete name) sim.forces

  -- Add/update each force in setup
  for_ setupConfig.forces \fc -> do
    -- Always create fresh handle to ensure params are up to date
    handle <- createForceHandle nodes links fc

    -- Initialize with current nodes (and links for link forces)
    _ <- case fc of
      LinkForce _ -> Core.initializeLinkForce handle nodes links
      _ -> Core.initializeForce handle nodes

    -- Store in simulation
    Ref.modify_ (Map.insert (forceName fc) handle) sim.forces

  -- Apply simulation params
  Ref.write setupConfig.params.alpha sim.alpha
  -- Note: Other params (alphaDecay, etc.) are stored in sim.config
  -- which is immutable after creation. For now we just update alpha.

-- | Create a D3 force handle from our config.
-- |
-- | What the D3 engine does with each parameter:
-- | - per node (`dynamic`): Collide radius, PositionX/PositionY target. A
-- |   `dynamic` strength (ManyBody, PositionX/Y, Radial) or Radial radius is
-- |   read as 0, as it always has been; the WASM engine evaluates them.
-- | - filters: ManyBody, Radial, PositionX, PositionY.
-- | - Link distance and strength: per link, and `ByDegree`.
createForceHandle
  :: forall row linkRow
   . Array (SimulationNode row)
  -> Array { source :: NodeID, target :: NodeID | linkRow }
  -> ForceConfig (SimulationNode row)
  -> Effect Core.ForceHandle
createForceHandle nodes links fc = do
  requireD3Support fc
  pure $ case fc of
    ManyBodyForce c -> case c.filter of
      -- Filtered variant - only applies to matching nodes
      Just predicate -> Core.createManyBodyFiltered
        { strength: valueToNumber c.strength
        , theta: c.theta
        , distanceMin: c.distanceMin
        , distanceMax: c.distanceMax
        , filter: predicate
        }
      -- Standard variant
      Nothing -> Core.createManyBody
        { strength: valueToNumber c.strength
        , theta: c.theta
        , distanceMin: c.distanceMin
        , distanceMax: c.distanceMax
        }

    CollideForce c -> case c.radius of
      Static r -> Core.createCollide
        { radius: r
        , strength: c.strength
        , iterations: c.iterations
        }
      Dynamic radiusFn -> Core.createCollideDynamic
        { radiusAccessor: radiusFn
        , strength: c.strength
        , iterations: c.iterations
        }

    LinkForce c -> case c.distance, c.strength of
      -- One distance and one strength for every link
      Static distance, LinkStrength (Static strength) -> Core.createLink
        { distance
        , strength
        , iterations: c.iterations
        }
      -- Otherwise one of each per link, evaluated here
      _, _ ->
        let
          ends = resolveLinks nodes links
        in
          Core.createLinkPerLink
            { distances: linkValues c.distance ends
            , strengths: linkStrengths c.strength ends
            , iterations: c.iterations
            }

    CenterForce c ->
      Core.createCenter
        { x: c.x
        , y: c.y
        , strength: c.strength
        }

    PositionXForce c -> case c.target, c.filter of
      Static x, Nothing -> Core.createForceX
        { x: x
        , strength: valueToNumber c.strength
        }
      Dynamic xFn, Nothing -> Core.createForceXDynamic
        { xAccessor: xFn
        , strength: valueToNumber c.strength
        }
      target, Just predicate -> Core.createForceXFiltered
        { xAccessor: targetOf target
        , strength: valueToNumber c.strength
        , filter: predicate
        }

    PositionYForce c -> case c.target, c.filter of
      Static y, Nothing -> Core.createForceY
        { y: y
        , strength: valueToNumber c.strength
        }
      Dynamic yFn, Nothing -> Core.createForceYDynamic
        { yAccessor: yFn
        , strength: valueToNumber c.strength
        }
      target, Just predicate -> Core.createForceYFiltered
        { yAccessor: targetOf target
        , strength: valueToNumber c.strength
        , filter: predicate
        }

    RadialForce c -> case c.filter of
      -- Filtered variant - only applies to matching nodes
      Just predicate -> Core.createRadialFiltered
        { radius: valueToNumber c.radius
        , x: c.x
        , y: c.y
        , strength: valueToNumber c.strength
        , filter: predicate
        }
      -- Standard variant
      Nothing -> Core.createRadial
        { radius: valueToNumber c.radius
        , x: c.x
        , y: c.y
        , strength: valueToNumber c.strength
        }

    -- Unreachable: requireD3Support has thrown for these. A force that
    -- does nothing, so that the case is total.
    TowardsForce _ -> inert
    RectCollideForce _ -> inert
    ContainForce _ -> inert
    CohesionForce _ -> inert
    DirectionalForce _ -> inert
  where
  inert = Core.createCenter { x: 0.0, y: 0.0, strength: 0.0 }

-- | A positioning target as a function of the node.
targetOf :: forall node. Value node Number -> node -> Number
targetOf = case _ of
  Static x -> const x
  Dynamic f -> f

-- =============================================================================
-- Incremental Operations (for fine-grained control)
-- =============================================================================

-- | Add a single force to simulation (without full setup).
-- | Throws if the D3 engine does not have the force.
addForceToSim
  :: forall row linkRow
   . ForceConfig (SimulationNode row)
  -> Simulation row linkRow
  -> Effect Unit
addForceToSim fc sim = do
  nodes <- Ref.read sim.nodes
  links <- Ref.read sim.links
  handle <- createForceHandle nodes links fc
  _ <- case fc of
    LinkForce _ -> Core.initializeLinkForce handle nodes links
    _ -> Core.initializeForce handle nodes
  Ref.modify_ (Map.insert (forceName fc) handle) sim.forces

-- | Remove a single force from simulation by name
removeForceFromSim
  :: forall row linkRow
   . String
  -> Simulation row linkRow
  -> Effect Unit
removeForceFromSim name sim = do
  Ref.modify_ (Map.delete name) sim.forces

-- =============================================================================
-- GUP (General Update Pattern) Types
-- =============================================================================

-- | Result of a node data update with enter/update/exit categorization
-- |
-- | - entered: Nodes newly added to simulation (use for enter animations)
-- | - updated: Existing nodes that were modified (simulation state preserved)
-- | - exited: Nodes removed from simulation (use for exit animations)
type GUPResult node =
  { entered :: Array node
  , updated :: Array node
  , exited :: Array node
  }

-- | Result of a link data update with enter/update/exit categorization
-- | Links are keyed by (source, target) pair
type GUPLinkResult linkRow =
  { entered :: Array { source :: NodeID, target :: NodeID | linkRow }
  , updated :: Array { source :: NodeID, target :: NodeID | linkRow }
  , exited :: Array { source :: NodeID, target :: NodeID | linkRow }
  }

-- =============================================================================
-- Apply Setup with Data (GUP semantics)
-- =============================================================================

-- | Apply setup with new nodes and links, using GUP (General Update Pattern) semantics.
-- |
-- | This function handles three concerns:
-- | 1. Force configuration (same as applySetup)
-- | 2. Node GUP: diff desired nodes vs current, merge simulation state for updates
-- | 3. Link GUP: diff desired links vs current
-- |
-- | Simulation state preservation (for updated nodes):
-- | - x, y: Current position (preserved)
-- | - vx, vy: Current velocity (preserved)
-- | - fx, fy: Pinned position (preserved)
-- | - All other fields: Taken from desired nodes
-- |
-- | Example usage:
-- | ```purescript
-- | -- Stop simulation before transition
-- | Sim.stop sim
-- |
-- | -- Apply new configuration and data
-- | result <- applySetupWithData mySetup newNodes newLinks sim
-- |
-- | -- Use result for animations
-- | animateEnter result.nodes.entered
-- | animateExit result.nodes.exited
-- |
-- | -- Restart simulation
-- | Sim.reheat sim
-- | ```
applySetupWithData
  :: forall row linkRow
   . Setup (SimulationNode row)
  -> Array (SimulationNode row)
  -> Array { source :: NodeID, target :: NodeID | linkRow }
  -> Simulation row linkRow
  -> Effect { nodes :: GUPResult (SimulationNode row), links :: GUPLinkResult linkRow }
applySetupWithData setupConfig desiredNodes desiredLinks sim = do
  -- Get current state
  currentNodes <- Ref.read sim.nodes
  currentLinks <- Ref.read sim.links

  -- Compute node GUP
  let nodeResult = computeNodeGUP currentNodes desiredNodes

  -- Write merged nodes to simulation
  Ref.write nodeResult.merged sim.nodes

  -- CRITICAL: Initialize node positions for any nodes that don't have them yet.
  -- D3 forces require nodes to have x, y, vx, vy for collision detection and
  -- force calculations to work. Without this, entered nodes have no positions
  -- and forces (especially collision) don't work.
  Core.initializeNodes nodeResult.merged

  -- Compute link GUP
  let linkResult = computeLinkGUP currentLinks desiredLinks

  -- Write desired links to simulation (links don't have simulation state to preserve)
  Ref.write desiredLinks sim.links

  -- Now apply force configuration (uses the new nodes/links)
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

-- =============================================================================
-- Node GUP Helpers
-- =============================================================================

-- | Internal result type that includes the merged array
type NodeGUPInternal node =
  { entered :: Array node
  , updated :: Array node
  , exited :: Array node
  , merged :: Array node  -- The actual array to write to simulation
  }

-- | Compute node GUP: which nodes are entering, updating, exiting
-- | For updates, merge simulation state (x, y, vx, vy, fx, fy) from current into desired
computeNodeGUP
  :: forall row
   . Array (SimulationNode row)
  -> Array (SimulationNode row)
  -> NodeGUPInternal (SimulationNode row)
computeNodeGUP currentNodes desiredNodes =
  let
    -- Build lookup map for current nodes by id
    currentById :: Map.Map NodeID (SimulationNode row)
    currentById = Map.fromFoldable $ map (\n -> Tuple n.id n) currentNodes

    -- Build set of desired ids (for computing exits)
    desiredIds = Set.fromFoldable $ map _.id desiredNodes

    -- Categorize and merge desired nodes
    processDesired :: SimulationNode row -> { node :: SimulationNode row, isEnter :: Boolean }
    processDesired desired =
      case Map.lookup desired.id currentById of
        Nothing ->
          -- Enter: use desired as-is
          { node: desired, isEnter: true }
        Just current ->
          -- Update: merge simulation state from current into desired
          { node: mergeSimulationState current desired, isEnter: false }

    processed = map processDesired desiredNodes

    -- Separate entered and updated
    entered = map _.node $ Array.filter _.isEnter processed
    updated = map _.node $ Array.filter (not <<< _.isEnter) processed

    -- Exited nodes (from current, not in desired)
    exited = Array.filter (\n -> not (Set.member n.id desiredIds)) currentNodes

    -- Merged array is all processed nodes (entered + updated, in desired order)
    merged = map _.node processed
  in
    { entered, updated, exited, merged }

-- | Merge simulation-managed state from current node into desired node
-- |
-- | Preserves: x, y, vx, vy, fx, fy (simulation-managed)
-- | Takes from desired: everything else (application data)
mergeSimulationState
  :: forall row
   . SimulationNode row
  -> SimulationNode row
  -> SimulationNode row
mergeSimulationState current desired =
  desired
    { x = current.x
    , y = current.y
    , vx = current.vx
    , vy = current.vy
    , fx = current.fx
    , fy = current.fy
    }

-- =============================================================================
-- Link GUP Helpers
-- =============================================================================

-- | Compute link GUP: which links are entering, updating, exiting
-- | Links are keyed by (source, target) pair
computeLinkGUP
  :: forall linkRow
   . Array { source :: NodeID, target :: NodeID | linkRow }
  -> Array { source :: NodeID, target :: NodeID | linkRow }
  -> GUPLinkResult linkRow
computeLinkGUP currentLinks desiredLinks =
  let
    -- Build key for a link
    linkKey :: { source :: NodeID, target :: NodeID | linkRow } -> Tuple NodeID NodeID
    linkKey l = Tuple l.source l.target

    -- Build sets of link keys
    currentKeys = Set.fromFoldable $ map linkKey currentLinks
    desiredKeys = Set.fromFoldable $ map linkKey desiredLinks

    -- Compute enter/update/exit key sets
    enterKeys = Set.difference desiredKeys currentKeys
    updateKeys = Set.intersection desiredKeys currentKeys
    exitKeys = Set.difference currentKeys desiredKeys

    -- Categorize
    entered = Array.filter (\l -> Set.member (linkKey l) enterKeys) desiredLinks
    updated = Array.filter (\l -> Set.member (linkKey l) updateKeys) desiredLinks
    exited = Array.filter (\l -> Set.member (linkKey l) exitKeys) currentLinks
  in
    { entered, updated, exited }
