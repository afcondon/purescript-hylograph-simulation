-- | Force Engine Core
-- |
-- | The simulation loop's parts: forces, the position update, cooling, the
-- | animation frame and dragging.
-- |
-- | The forces are `hylograph-forces`: d3-force's seven and the Rust
-- | kernel's additions, in PureScript, with no D3 anywhere. A force is made in
-- | two steps, because a simulation is usually described before its nodes
-- | exist:
-- |
-- | 1. a `ForceRecipe` says what the force is (`createManyBody config`, or
-- |    `Hylograph.ForceEngine.Setup.recipe` for anything a `Setup` can say);
-- | 2. `initializeForce` gives it the nodes and links and returns a
-- |    `ForceHandle`, which `applyForce` applies each tick.
-- |
-- | Per-node and per-link values are read when the force is initialized, so
-- | a force has to be initialized again (`reinitializeForce`) when the nodes,
-- | the links, or anything those values are computed from have changed.
-- |
-- | Coincident nodes are separated by a seeded generator (`RandomSource`),
-- | one per simulation, so the same nodes, links and forces give the same
-- | layout every time.
module Hylograph.ForceEngine.Core
  ( -- * Nodes and links as the forces see them
    Node
  , NodeRow
  , Links
    -- * Forces
  , ForceRecipe(..)
  , ForceHandle
  , RandomSource
  , randomSource
  , createManyBody
  , createCollide
  , createLink
  , createCenter
  , createForceX
  , createForceY
  , createRadial
  , createForceXGrid
  , createForceYGrid
  , createCollideGrid
    -- * Initialization
  , initializeNodes
  , initializeForce
  , reinitializeForce
    -- * Simulation Step
  , applyForce
  , applyForces
  , integratePositions
  , constrain
  , decayAlpha
  , simulationTick
    -- * Animation
  , AnimationHandle
  , startAnimation
  , stopAnimation
    -- * Drag Behavior
  , attachDragWithReheat
  , attachGroupDragWithReheat
  , attachPinningDrag
    -- * DOM Utilities
  , querySelectorElements
    -- * Debug
  , logNodes
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (for_)
import Effect (Effect)
import Hylograph.ForceEngine.Types (ManyBodyConfig, CollideConfig, LinkConfig, CenterConfig, ForceXConfig, ForceYConfig, RadialConfig)
import Hylograph.Forces as Pure
import Hylograph.Forces.Group (ungrouped)
import Hylograph.Forces.Link as PureLink
import Hylograph.Forces.ManyBody (everyPair)
import Hylograph.Forces.Node as PureNode
import Hylograph.Forces.Random as Random
import Hylograph.Forces.Tick as Tick
import Hylograph.Interaction.Pointer as Pointer
import Web.DOM.Element (Element)

-- =============================================================================
-- Nodes and links
-- =============================================================================

-- | What a node has beyond position and velocity: an id, and the pins.
-- | `Record (NodeRow row)` plus x, y, vx, vy is
-- | `Hylograph.ForceEngine.Simulation.SimulationNode row`.
type NodeRow row = PureNode.Pinnable (id :: Int | row)

-- | A simulation node. The same type as `SimulationNode row`.
type Node row = PureNode.Node (NodeRow row)

-- | Links, naming their nodes by id.
type Links = Array { source :: Int, target :: Int }

-- =============================================================================
-- Forces
-- =============================================================================

-- | The seeded generator that separates coincident nodes: d3's own, started
-- | where d3 starts it.
type RandomSource = Random.Source

-- | A fresh generator, for a new simulation.
randomSource :: Effect RandomSource
randomSource = Random.lcg

-- | What a force is, before it has met its nodes: given the simulation's
-- | generator, nodes and links, the force itself, or why it cannot be made
-- | (a link naming a node that is not there: `node not found: <id>`).
newtype ForceRecipe row = ForceRecipe
  (RandomSource -> Array (Node row) -> Links -> Either String (Pure.Force (NodeRow row)))

-- | A force that has its nodes, and the recipe it was made from, so that it
-- | can be made again when they change.
newtype ForceHandle row = ForceHandle
  { recipe :: ForceRecipe row
  , force :: Pure.Force (NodeRow row)
  }

-- | A many-body (charge) force with one strength for every node.
createManyBody :: forall row. ManyBodyConfig -> ForceRecipe row
createManyBody config = ForceRecipe \random nodes _ ->
  Right $ Pure.manyBody
    { strength: \_ -> config.strength
    , theta: config.theta
    , distanceMin: config.distanceMin
    , distanceMax: config.distanceMax
    , scope: everyPair
    , group: \_ -> ungrouped
    }
    random
    nodes

-- | A collision force with one radius for every node.
createCollide :: forall row. CollideConfig -> ForceRecipe row
createCollide config = ForceRecipe \random nodes _ ->
  Right $ Pure.collide
    { radius: \_ -> config.radius
    , strength: config.strength
    , iterations: config.iterations
    , include: \_ -> true
    , alphaScaled: false
    }
    random
    nodes

-- | A link force with one distance and one strength for every link.
createLink :: forall row. LinkConfig -> ForceRecipe row
createLink config = ForceRecipe \random nodes links ->
  Pure.linkById _.id
    { distance: \_ -> config.distance
    , strength: PureLink.strengthPerLink \_ -> config.strength
    , iterations: config.iterations
    }
    random
    nodes
    links

-- | A centering force.
createCenter :: forall row. CenterConfig -> ForceRecipe row
createCenter config = ForceRecipe \_ nodes _ -> Right $ Pure.center config nodes

-- | A force pulling every node toward one x.
createForceX :: forall row. ForceXConfig -> ForceRecipe row
createForceX config = ForceRecipe \_ nodes _ ->
  Right $ Pure.positionX { target: \_ -> config.x, strength: \_ -> config.strength } nodes

-- | A force pulling every node toward one y.
createForceY :: forall row. ForceYConfig -> ForceRecipe row
createForceY config = ForceRecipe \_ nodes _ ->
  Right $ Pure.positionY { target: \_ -> config.y, strength: \_ -> config.strength } nodes

-- | A force pulling every node toward one circle.
createRadial :: forall row. RadialConfig -> ForceRecipe row
createRadial config = ForceRecipe \_ nodes _ ->
  Right $ Pure.radial
    { radius: \_ -> config.radius, strength: \_ -> config.strength, x: config.x, y: config.y }
    nodes

-- | A force pulling each node toward its own `gridX`, read off the node when
-- | the force is initialized.
createForceXGrid :: forall row. Number -> ForceRecipe (gridX :: Number | row)
createForceXGrid strength = ForceRecipe \_ nodes _ ->
  Right $ Pure.positionX { target: _.gridX, strength: \_ -> strength } nodes

-- | A force pulling each node toward its own `gridY`.
createForceYGrid :: forall row. Number -> ForceRecipe (gridY :: Number | row)
createForceYGrid strength = ForceRecipe \_ nodes _ ->
  Right $ Pure.positionY { target: _.gridY, strength: \_ -> strength } nodes

-- | A collision force whose radius is each node's own `r` plus a padding.
-- | Arguments: padding, strength, iterations.
createCollideGrid :: forall row. Number -> Number -> Int -> ForceRecipe (r :: Number | row)
createCollideGrid padding strength iterations = ForceRecipe \random nodes _ ->
  Right $ Pure.collide
    { radius: \node -> node.r + padding
    , strength
    , iterations
    , include: \_ -> true
    , alphaScaled: false
    }
    random
    nodes

-- =============================================================================
-- Initialization
-- =============================================================================

-- | d3's `initializeNodes`: give every node its index, put a pinned node on
-- | its pin, stop a node with no velocity, and give a node with no position
-- | a place on d3's phyllotaxis spiral. (Until 2026-11-18 a node with no
-- | position was put somewhere at random.)
initializeNodes :: forall row. Array (Node row) -> Effect Unit
initializeNodes = Tick.initializeNodes

-- | Make a force for these nodes and links.
initializeForce
  :: forall row
   . RandomSource
  -> Array (Node row)
  -> Links
  -> ForceRecipe row
  -> Either String (ForceHandle row)
initializeForce random nodes links recipe = case recipe of
  ForceRecipe make -> map (\force -> ForceHandle { recipe, force }) (make random nodes links)

-- | Make a force again, from the recipe it was first made from, for nodes
-- | or links that have changed.
reinitializeForce
  :: forall row
   . RandomSource
  -> Array (Node row)
  -> Links
  -> ForceHandle row
  -> Either String (ForceHandle row)
reinitializeForce random nodes links handle = case handle of
  ForceHandle made -> initializeForce random nodes links made.recipe

-- =============================================================================
-- Simulation Step
-- =============================================================================

-- | Apply a single force: it changes the velocities (or, for centering, the
-- | positions) of the nodes it was initialized with.
applyForce :: forall row. ForceHandle row -> Number -> Effect Unit
applyForce handle alpha = case handle of
  ForceHandle made -> Pure.applyForce alpha made.force

-- | Apply forces in order, each seeing what the ones before it did.
applyForces :: forall row. Array (ForceHandle row) -> Number -> Effect Unit
applyForces forces alpha = for_ forces \force -> applyForce force alpha

-- | Move the nodes: each velocity is multiplied by `velocityRetention` and
-- | added to the position, and a pinned coordinate goes to its pin and stops.
-- | Call once per tick, after the forces.
integratePositions :: forall row. Array (Node row) -> Number -> Effect Unit
integratePositions nodes velocityRetention = Tick.integrate velocityRetention nodes

-- | After the nodes have moved: every hard containment clamps its nodes into
-- | their regions, and then pinned coordinates are put back (a pin beats a
-- | region). Nothing where no force is a hard containment.
constrain :: forall row. Array (ForceHandle row) -> Array (Node row) -> Effect Unit
constrain forces nodes = Pure.constrain (map (\(ForceHandle made) -> made.force) forces) nodes

-- | Cool: alpha moves toward `alphaTarget` by `alphaDecay` of the way, and
-- | is zero once it has fallen below `alphaMin`, which is how the loop knows
-- | to stop. Arguments: alpha, alphaMin, alphaDecay, alphaTarget.
decayAlpha :: Number -> Number -> Number -> Number -> Number
decayAlpha alpha alphaMin alphaDecay alphaTarget = Tick.decay { alphaMin, alphaDecay, alphaTarget } alpha

-- | A complete tick: apply the forces at the current alpha, move the nodes,
-- | apply hard containment, and return the cooled alpha.
simulationTick
  :: forall row
   . { forces :: Array (ForceHandle row)
     , nodes :: Array (Node row)
     , alpha :: Number
     , alphaMin :: Number
     , alphaDecay :: Number
     , alphaTarget :: Number
     , velocityRetention :: Number
     }
  -> Effect Number
simulationTick params = do
  applyForces params.forces params.alpha
  integratePositions params.nodes params.velocityRetention
  constrain params.forces params.nodes
  pure $ decayAlpha params.alpha params.alphaMin params.alphaDecay params.alphaTarget

-- =============================================================================
-- Animation Loop
-- =============================================================================

foreign import requestAnimationFrame_ :: (Number -> Effect Unit) -> Effect (Effect Unit)

-- | Handle to a running animation (can be used to stop it)
type AnimationHandle = Effect Unit

-- | Start an animation loop
-- | The callback receives the current timestamp and should return whether to continue
startAnimation :: (Number -> Effect Boolean) -> Effect AnimationHandle
startAnimation onFrame = do
  cancelRef <- newCancelRef
  let
    loop timestamp = do
      continue <- onFrame timestamp
      when continue do
        cancel <- requestAnimationFrame_ loop
        setCancelRef cancelRef cancel
  cancel <- requestAnimationFrame_ loop
  setCancelRef cancelRef cancel
  pure $ getCancelRef cancelRef >>= identity

foreign import newCancelRef :: Effect CancelRef
foreign import setCancelRef :: CancelRef -> Effect Unit -> Effect Unit
foreign import getCancelRef :: CancelRef -> Effect (Effect Unit)
foreign import data CancelRef :: Type

-- | Stop a running animation
stopAnimation :: AnimationHandle -> Effect Unit
stopAnimation cancel = cancel

-- =============================================================================
-- Drag Behavior
-- =============================================================================
--
-- Native Pointer Events throughout; no d3-drag. Each element's `__data__`
-- is the simulation node it stands for (the convention the selection
-- library keeps).

-- | Drag nodes: reheat when a drag starts, pin the node under the pointer
-- | while it lasts, release it when it ends. This is
-- | `Hylograph.Interaction.Pointer.attachSimulationDrag` from
-- | hylograph-selection, attached to each element.
attachDragWithReheat :: Array Element -> Effect Unit -> Effect Unit
attachDragWithReheat elements reheat =
  for_ elements \element -> void $ Pointer.attachSimulationDrag element reheat

foreign import attachGroupDragWithReheat_ :: Array Element -> String -> Effect Unit -> Effect Unit

-- | Drag transformed group elements (like bubble packs)
-- |
-- | Unlike `attachDragWithReheat`, this takes a container selector and reads
-- | the pointer in that container's coordinates. Use it when dragging `<g>`
-- | elements that have `transform` attributes. The node is the datum of the
-- | nearest ancestor of what was pressed that has the class `module-pack`.
attachGroupDragWithReheat :: Array Element -> String -> Effect Unit -> Effect Unit
attachGroupDragWithReheat = attachGroupDragWithReheat_

foreign import attachPinningDrag_ :: Array Element -> Effect Unit -> Effect Unit

-- | Drag with toggle-pinning behavior
-- |
-- | Unlike `attachDragWithReheat`, a node stays pinned where it is dropped.
-- | Pressing a pinned node and releasing it within 3px unpins it.
attachPinningDrag :: Array Element -> Effect Unit -> Effect Unit
attachPinningDrag = attachPinningDrag_

-- =============================================================================
-- DOM Utilities
-- =============================================================================

foreign import querySelectorElements_ :: String -> Effect (Array Element)

-- | Query DOM elements by CSS selector
querySelectorElements :: String -> Effect (Array Element)
querySelectorElements = querySelectorElements_

-- =============================================================================
-- Debug
-- =============================================================================

foreign import logNodes_ :: forall r. String -> Array { | r } -> Effect Unit

-- | Log node positions for debugging
logNodes :: forall r. String -> Array { | r } -> Effect Unit
logNodes = logNodes_
