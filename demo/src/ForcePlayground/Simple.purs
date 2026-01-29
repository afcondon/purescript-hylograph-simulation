-- | Simple Force Layout for Network Datasets - Using High-Level runSimulation API
-- |
-- | This module provides stateless functions for force-directed graph visualization.
-- | All state lives in Halogen; this module only provides:
-- | - Simulation creation (via runSimulation)
-- | - Scene data construction
-- | - Rendering
-- | - Force configuration (via Setup DSL)
-- |
-- | The Halogen component owns:
-- | - The model (nodes/links data)
-- | - The simulation handle (opaque)
-- | - Enabled forces set
-- | - Visible node IDs
-- | - Enter/exit transition state
module ForcePlayground.Simple
  ( -- Simulation creation
    createSimulation
  , SimulationResult
  , NetworkSimulationHandle
  , NetworkNodeRow
  , NetworkLinkRow
  -- Scene data
  , SceneData
  , RenderNode
  , SwizzledLink
  , ExitingNode
  , buildSceneData
  -- Rendering
  , renderSVGContainer
  , renderScene
  -- Force configuration (Setup DSL)
  , ForceId(..)
  , forceIdToName
  , allForces
  , initialSetup
  , addForceToSetup
  , removeForceFromSetup
  , forceIdToConfig
  -- Visual helpers
  , nodeRadius
  , categoryColor
  , linkTypeColor
  -- Transition timing
  , transitionDelta
  -- Simulation registration
  , simulationId
  , registerSimulation
  , unregisterSimulation
  ) where

import Prelude

import Data.Array (filter, elem, index)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import ForcePlayground.Model (NetworkModel, NetworkNode)
import Hylograph.Kernel.D3.Links (swizzleLinksByIndex, filterLinksToSubset)
import Hylograph.Simulation.Core.Tick as Tick
import Hylograph.HATS (elem, forEach, withBehaviors, onDrag) as HATS
import Hylograph.HATS (Tree) as HTree
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (rerender, clearContainer) as HATS
import Hylograph.Internal.Behavior.Types (DragConfig(..), simulationDragNested)
import Hylograph.Internal.Behavior.FFI (registerSimulation_, unregisterSimulation_)
import Hylograph.Internal.Selection.Types (ElementType(..))
-- Old AST import - only for nodeTemplate passed to runSimulation (not actually used when renderNodes: false)
import Hylograph.AST as A
import Hylograph.Expr.Friendly (num, text, attr, cx, cy, r, fill, stroke, strokeWidth, opacity) as OldF

-- High-level simulation imports
import Hylograph.Simulation (runSimulation, Engine(..), SimulationHandle)
import Hylograph.Simulation.Emitter (SimulationEmitter)
import Hylograph.ForceEngine.Setup (Setup, setup, manyBody, collide, link, positionX, positionY, withStrength, withRadius, withDistance, withX, withY, static, addForce, removeForce)
import Hylograph.ForceEngine.Simulation (SimulationNode)

-- =============================================================================
-- Type Definitions
-- =============================================================================

-- | Row types for the simulation (must match extra fields in NetworkNode/NetworkLink)
type NetworkNodeRow = (name :: String, group :: Int, sizeClass :: Int, importance :: Number, subgraph :: Int)
type NetworkLinkRow = (weight :: Number, linkType :: Int, distance :: Number)

-- | Handle type for the simulation
type NetworkSimulationHandle = SimulationHandle NetworkNodeRow

-- | Result from creating a simulation
type SimulationResult =
  { handle :: NetworkSimulationHandle
  , events :: SimulationEmitter
  , initialSetup :: Setup (SimulationNode NetworkNodeRow)
  }

-- | Exiting node with frozen position and transition progress
type ExitingNode = Tick.Transitioning NetworkNode

-- | Node ready for rendering with computed visual state
type RenderNode =
  { node :: NetworkNode
  , enterProgress :: Maybe Number  -- Just 0.0-1.0 if entering, Nothing if not
  , exitProgress :: Maybe Number   -- Just 0.0-1.0 if exiting, Nothing if not
  }

-- | Swizzled link for rendering (with full node refs and link attributes)
type SwizzledLink =
  { source :: NetworkNode
  , target :: NetworkNode
  , weight :: Number
  , linkType :: Int
  , index :: Int
  }

-- | Scene data for rendering - pure data structure
type SceneData =
  { nodes :: Array RenderNode
  , links :: Array SwizzledLink
  }

-- =============================================================================
-- Force Configuration (Setup DSL)
-- =============================================================================

-- | Identifiers for the forces we use
data ForceId
  = ForceCharge
  | ForceCollide
  | ForceLink
  | ForceX
  | ForceY

derive instance eqForceId :: Eq ForceId
derive instance ordForceId :: Ord ForceId

-- | Get the string name for a force (matches D3 force names)
forceIdToName :: ForceId -> String
forceIdToName = case _ of
  ForceCharge -> "charge"
  ForceCollide -> "collision"
  ForceLink -> "links"
  ForceX -> "forceX"
  ForceY -> "forceY"

-- | All forces enabled by default
allForces :: Set ForceId
allForces = Set.fromFoldable [ForceX, ForceY, ForceCharge, ForceCollide, ForceLink]

-- | Create the force config for a specific force ID
forceIdToConfig :: forall node. ForceId -> Setup node -> Setup node
forceIdToConfig forceId = case forceId of
  ForceCharge ->
    addForce (manyBody "charge" # withStrength (static (-30.0)))
  ForceCollide ->
    addForce (collide "collision" # withRadius (static 10.0) # withStrength (static 0.7))
  ForceLink ->
    addForce (link "links" # withDistance (static 40.0) # withStrength (static 0.4))
  ForceX ->
    addForce (positionX "forceX" # withX (static 0.0) # withStrength (static 0.1))
  ForceY ->
    addForce (positionY "forceY" # withY (static (-50.0)) # withStrength (static 0.1))

-- | Initial setup with all forces enabled
initialSetup :: Setup (SimulationNode NetworkNodeRow)
initialSetup =
  setup "network"
    [ manyBody "charge" # withStrength (static (-30.0))
    , collide "collision" # withRadius (static 10.0) # withStrength (static 0.7)
    , link "links" # withDistance (static 40.0) # withStrength (static 0.4)
    , positionX "forceX" # withX (static 0.0) # withStrength (static 0.1)
    , positionY "forceY" # withY (static (-50.0)) # withStrength (static 0.1)
    ]

-- | Add a force to the setup
addForceToSetup :: ForceId -> Setup (SimulationNode NetworkNodeRow) -> Setup (SimulationNode NetworkNodeRow)
addForceToSetup = forceIdToConfig

-- | Remove a force from the setup by name
removeForceFromSetup :: ForceId -> Setup (SimulationNode NetworkNodeRow) -> Setup (SimulationNode NetworkNodeRow)
removeForceFromSetup forceId = removeForce (forceIdToName forceId)

-- =============================================================================
-- Constants
-- =============================================================================

-- | Simulation ID for drag behavior registration
simulationId :: String
simulationId = "force-playground"

-- | SVG dimensions - larger for fullscreen
svgWidth :: Number
svgWidth = 1200.0

svgHeight :: Number
svgHeight = 900.0

-- | Transition speed: progress increment per tick
-- | At 60fps, 0.03 ≈ 33 ticks ≈ 0.55 seconds (snappy)
transitionDelta :: Tick.TickDelta
transitionDelta = 0.03

-- =============================================================================
-- Simulation Registration (for drag behavior)
-- =============================================================================

-- | Register the simulation for drag behavior
-- | Takes the reheat function from the handle
registerSimulation :: Effect Unit -> Effect Unit
registerSimulation reheatFn = registerSimulation_ simulationId reheatFn

-- | Unregister the simulation (call on cleanup)
unregisterSimulation :: Effect Unit
unregisterSimulation = unregisterSimulation_ simulationId

-- =============================================================================
-- Simulation Creation
-- =============================================================================

-- | Node template for runSimulation API (not used when renderNodes: false)
-- | Uses old AST to match runSimulation's expected type signature
networkNodeTemplate :: SimulationNode NetworkNodeRow -> A.Tree (SimulationNode NetworkNodeRow)
networkNodeTemplate node =
  A.elem Circle
    [ OldF.cx $ OldF.num node.x
    , OldF.cy $ OldF.num node.y
    , OldF.r $ OldF.num (nodeRadius node)
    , OldF.fill $ OldF.text (categoryColor node.group)
    , OldF.stroke $ OldF.text "#fff"
    , OldF.strokeWidth $ OldF.num 1.5
    , OldF.opacity $ OldF.num 1.0
    , OldF.attr "style" $ OldF.text "cursor: grab;"
    ]

-- | Create a simulation using the high-level runSimulation API
-- |
-- | Returns both handle and event emitter.
-- | Note: renderNodes is false because ForcePlayground handles rendering
-- | via tick-driven transitions with enter/exit animations.
createSimulation :: NetworkModel -> Effect SimulationResult
createSimulation model = do
  let
    -- Convert model links to the format expected by runSimulation
    simLinks = map (\l -> { source: l.source, target: l.target }) model.links

    -- Configuration for runSimulation
    config =
      { engine: D3
      , setup: initialSetup
      , nodes: model.nodes
      , links: simLinks
      , container: "#network-nodes"
      , alphaMin: 0.001
      }

  -- Run the simulation
  result <- runSimulation config

  pure
    { handle: result.handle
    , events: result.events
    , initialSetup: initialSetup
    }

-- =============================================================================
-- Scene Data Construction
-- =============================================================================

-- | Build scene data from current simulation state and Halogen state
-- |
-- | Arguments:
-- | - allNodes: All nodes from simulation (with current positions)
-- | - visibleIds: Array of node IDs that should be visible
-- | - enteringProgress: Map of node ID -> progress (0-1) for nodes entering
-- | - exitingNodes: Array of nodes with frozen positions that are exiting
-- | - modelLinks: Links from the model (not simulation)
buildSceneData
  :: Array NetworkNode
  -> Array Int
  -> Map Int Tick.Progress
  -> Array ExitingNode
  -> Array { source :: Int, target :: Int, weight :: Number, linkType :: Int, distance :: Number }
  -> SceneData
buildSceneData allNodes visibleIds enteringProgress exitingNodes modelLinks =
  let
    -- Filter to visible nodes (those in the visibleIds list)
    visibleNodes = filter (\n -> elem n.id visibleIds) allNodes

    -- Create render nodes with enter/exit transition state
    renderNodes = map
      (\n ->
        { node: n
        , enterProgress: Map.lookup n.id enteringProgress
        , exitProgress: Nothing
        }
      )
      visibleNodes

    -- Add exiting nodes (with frozen positions and exit progress)
    exitingRenderNodes = map
      (\e ->
        { node: e.item
        , enterProgress: Nothing
        , exitProgress: Just e.progress
        }
      )
      exitingNodes

    allRenderNodes = renderNodes <> exitingRenderNodes

    -- Create links only between visible nodes (not exiting)
    visibleLinks = filterLinksToSubset _.id visibleNodes modelLinks
    swizzledLinks = swizzleLinksByIndex _.id visibleNodes visibleLinks \src tgt i lnk ->
      { source: src, target: tgt, weight: lnk.weight, linkType: lnk.linkType, index: i }
  in
    { nodes: allRenderNodes, links: swizzledLinks }

-- =============================================================================
-- Rendering (Side-effecting but stateless)
-- =============================================================================

-- | Render the SVG container structure (no zoom/pan - nodes are draggable instead)
renderSVGContainer :: String -> Effect Unit
renderSVGContainer containerSelector = do
  let containerTree :: HTree.Tree
      containerTree =
        HATS.elem SVG
          [ F.width svgWidth
          , F.height svgHeight
          , F.viewBox ((-svgWidth) / 2.0) ((-svgHeight) / 2.0) svgWidth svgHeight
          , F.attr "id" "network-force-svg"
          , F.class_ "network-force"
          , F.preserveAspectRatio "xMidYMid meet"
          ]
          [ HATS.elem Group
              [ F.attr "id" "network-links", F.class_ "links" ]
              []
          , HATS.elem Group
              [ F.attr "id" "network-nodes", F.class_ "nodes" ]
              []
          ]

  _ <- HATS.rerender containerSelector containerTree
  pure unit

-- | Render the scene (nodes and links)
renderScene :: SceneData -> Effect Unit
renderScene scene = do
  -- Render links
  let linksTree = createLinksTree scene
  _ <- HATS.rerender "#network-links" linksTree

  -- Render nodes
  let nodesTree = createNodesTree scene
  _ <- HATS.rerender "#network-nodes" nodesTree

  pure unit

-- =============================================================================
-- Visual Property Helpers
-- =============================================================================

-- | Color palette for node categories (Alien Semiotic Standard inspired)
categoryColors :: Array String
categoryColors =
  [ "#1e5a8c"  -- blue (Research) - cryogenic/technical
  , "#d4a024"  -- amber (Industry) - caution/industrial
  , "#c41e3a"  -- red (Government) - warning/critical
  , "#2d8b57"  -- green (Community) - organic/life support
  , "#6b7280"  -- gray - neutral
  , "#9333ea"  -- purple - special
  , "#f97316"  -- orange
  , "#94a3b8"  -- slate
  , "#dc2626"  -- red accent
  , "#0891b2"  -- cyan
  ]

-- | Get color for a category/group
categoryColor :: Int -> String
categoryColor g = fromMaybe "#69b3a2" (index categoryColors (g `mod` 10))

-- | Color palette for link types (muted versions of node colors)
linkTypeColors :: Array String
linkTypeColors =
  [ "#4a7ba8"  -- muted blue (Collaboration)
  , "#b8912a"  -- muted amber (Citation)
  , "#a84858"  -- muted red (Funding)
  , "#4a9070"  -- muted green (Communication)
  ]

-- | Get color for a link type
linkTypeColor :: Int -> String
linkTypeColor t = fromMaybe "#999" (index linkTypeColors (t `mod` 4))

-- | Calculate node radius based on sizeClass and importance
nodeRadius :: NetworkNode -> Number
nodeRadius node =
  let
    baseSize = case node.sizeClass of
      0 -> 3.0   -- small
      1 -> 5.0   -- medium
      _ -> 7.0   -- large
    -- Importance adds 0-3 extra pixels
    importanceBonus = node.importance * 3.0
  in
    baseSize + importanceBonus

-- =============================================================================
-- Tree Construction for Rendering
-- =============================================================================

-- | Create nodes tree with transition-aware visual properties and drag behavior
createNodesTree :: SceneData -> HTree.Tree
createNodesTree scene =
  HATS.forEach "nodes" Circle scene.nodes nodeKey \rn ->
    HATS.withBehaviors
      [ HATS.onDrag (simulationDragNested simulationId) ]
    $ HATS.elem Circle
        [ F.cx rn.node.x
        , F.cy rn.node.y
        , F.r (radiusForRenderNode rn)
        , F.fill (fillForRenderNode rn)
        , F.stroke (strokeForRenderNode rn)
        , F.strokeWidth (strokeWidthForRenderNode rn)
        , F.opacity (show (opacityForRenderNode rn))
        , F.style "cursor: grab;"
        ]
        []
  where
  nodeKey :: RenderNode -> String
  nodeKey rn = show rn.node.id

-- | Create links tree with rich visual encoding
createLinksTree :: SceneData -> HTree.Tree
createLinksTree scene =
  HATS.forEach "links" Line scene.links linkKey \lnk ->
    HATS.elem Line
      [ F.x1 lnk.source.x
      , F.y1 lnk.source.y
      , F.x2 lnk.target.x
      , F.y2 lnk.target.y
      , F.strokeWidth (0.5 + lnk.weight * 2.0)  -- 0.5-2.5 based on weight
      , F.stroke (linkTypeColor lnk.linkType)
      , F.opacity (show (0.3 + lnk.weight * 0.4))  -- 0.3-0.7 based on weight
      ]
      []
  where
  linkKey :: SwizzledLink -> String
  linkKey lnk = show lnk.source.id <> "-" <> show lnk.target.id <> "-" <> show lnk.index

-- =============================================================================
-- Visual Property Interpolation (Tick-Driven)
-- =============================================================================

-- | Get radius for a node based on transition state
radiusForRenderNode :: RenderNode -> Number
radiusForRenderNode rn =
  let baseRadius = nodeRadius rn.node
  in case rn.enterProgress, rn.exitProgress of
    Just p, _ -> Tick.lerp (baseRadius * 3.0) baseRadius p  -- Entering: large → normal
    _, Just p -> Tick.lerp baseRadius (baseRadius * 3.0) p  -- Exiting: normal → large (pop)
    _, _ -> baseRadius

-- | Get fill color for a node based on transition state
fillForRenderNode :: RenderNode -> String
fillForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just _, _ -> "#2ca02c"  -- Green flash for entering
    _, Just _ -> "#d62728"  -- Red flash for exiting
    _, _ -> categoryColor rn.node.group  -- Normal category color

-- | Get stroke color for a node based on transition state
strokeForRenderNode :: RenderNode -> String
strokeForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just _, _ -> "#98df8a"  -- Light green for entering
    _, Just _ -> "#ff9896"  -- Light red for exiting
    _, _ -> "#fff"

-- | Get stroke width for a node based on transition state
strokeWidthForRenderNode :: RenderNode -> Number
strokeWidthForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just p, _ -> Tick.lerp 4.0 1.5 p  -- Thick → normal
    _, Just p -> Tick.lerp 1.5 4.0 p  -- Normal → thick
    _, _ -> 1.5

-- | Get opacity for a node based on transition state
opacityForRenderNode :: RenderNode -> Number
opacityForRenderNode rn =
  case rn.exitProgress of
    Just p -> Tick.lerp 1.0 0.0 p  -- Fade out during exit
    _ -> 1.0  -- Full opacity
