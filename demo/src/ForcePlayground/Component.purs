-- | Force Playground Component - Using High-Level runSimulation API
-- |
-- | This component demonstrates the high-level `runSimulation` API for
-- | force-directed graph visualization. It provides:
-- |
-- | - Interactive force configuration (toggle individual forces)
-- | - Preset layouts (Centered, Floating, Quadrants)
-- | - Category filtering with animated transitions
-- | - Drag interaction for node repositioning
-- |
-- | Architecture:
-- | - Uses `runSimulation` from Hylograph.Simulation
-- | - Uses `toHalogenEmitter` for Halogen subscriptions
-- | - Uses `handle.updateSetup` for force toggling
-- | - Uses `handle.interpolatePositions` for animated quadrant layout
module ForcePlayground.Component where

import Prelude

import Data.Array (filter, length, mapWithIndex)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Path as SP
import Halogen.Svg.Elements as SE
import Data.Number (cos, pi, sin, sqrt)
import Hylograph.Transform (clearContainer)
import ForcePlayground.Model (NetworkModel, fromGeneratedGraph)
import ForcePlayground.Simple as Simple
import ForcePlayground.Simple (ForceId(..), NetworkSimulationHandle, ExitingNode, renderSVGContainer, renderScene, buildSceneData)

import ForcePlayground.Generator as Gen
import Hylograph.Simulation (SimulationEvent(..), PositionMap)
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)
import Hylograph.ForceEngine.Setup (Setup)
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.Simulation.Core.Tick as Tick

-- =============================================================================
-- Force Presets
-- =============================================================================

-- | Force presets - pre-configured combinations
data ForcePreset
  = PresetStandard   -- All forces enabled (default)
  | PresetClustered  -- No X/Y centering - clusters drift apart
  | PresetQuadrants  -- Animated cluster by color to quadrants

derive instance eqForcePreset :: Eq ForcePreset

presetLabel :: ForcePreset -> String
presetLabel = case _ of
  PresetStandard -> "Centered"
  PresetClustered -> "Floating"
  PresetQuadrants -> "Quadrants"

presetTooltip :: ForcePreset -> String
presetTooltip = case _ of
  PresetStandard -> "All forces enabled - nodes pulled to center"
  PresetClustered -> "No centering - clusters float freely"
  PresetQuadrants -> "Animate nodes to color-coded quadrants"

presetForces :: ForcePreset -> Set ForceId
presetForces = case _ of
  PresetStandard -> Simple.allForces
  PresetClustered -> Set.fromFoldable [ForceCharge, ForceCollide, ForceLink]
  PresetQuadrants -> Set.fromFoldable [ForceCollide]  -- Only collision while animating

-- =============================================================================
-- Quadrant Layout Computation
-- =============================================================================

-- | Target position for quadrant animation
type TargetPosition = { x :: Number, y :: Number }

-- | Quadrant centers (offset from origin to avoid nav)
-- | NW=Research(blue), NE=Industry(amber), SW=Government(red), SE=Community(green)
quadrantCenter :: Int -> { x :: Number, y :: Number }
quadrantCenter group = case group `mod` 4 of
  0 -> { x: -200.0, y: -150.0 }  -- NW - Research (blue)
  1 -> { x:  200.0, y: -150.0 }  -- NE - Industry (amber)
  2 -> { x: -200.0, y:  150.0 }  -- SW - Government (red)
  _ -> { x:  200.0, y:  150.0 }  -- SE - Community (green)

-- | Golden angle for phyllotaxis (radians)
goldenAngle :: Number
goldenAngle = pi * (3.0 - sqrt 5.0)

-- | Compute phyllotaxis position around a center point
-- | index: node's index within its category (0, 1, 2, ...)
-- | This spreads nodes in a sunflower pattern to avoid overlapping starts
phyllotaxisOffset :: Int -> { x :: Number, y :: Number }
phyllotaxisOffset idx =
  let
    i = toNumber idx
    angle = i * goldenAngle
    radius = 8.0 * sqrt i  -- Spacing increases outward
  in
    { x: radius * cos angle
    , y: radius * sin angle
    }

-- | Compute target positions for all nodes based on their category
-- | Groups nodes by category and applies phyllotaxis within each group
computeQuadrantTargets :: Array { id :: Int, group :: Int | _ } -> Map Int TargetPosition
computeQuadrantTargets nodes =
  let
    -- Group nodes by category
    grouped = Array.groupBy (\a b -> a.group == b.group) $
              Array.sortWith _.group nodes

    -- For each group, compute phyllotaxis positions around quadrant center
    computeGroupTargets :: NEA.NonEmptyArray { id :: Int, group :: Int | _ } -> Array (Tuple Int TargetPosition)
    computeGroupTargets groupNodes =
      let first = NEA.head groupNodes
          center = quadrantCenter first.group
          groupArray = NEA.toArray groupNodes
      in mapWithIndex (\idx n ->
           let offset = phyllotaxisOffset idx
           in Tuple n.id { x: center.x + offset.x, y: center.y + offset.y }
         ) groupArray

    allTargets = Array.concatMap computeGroupTargets grouped
  in
    Map.fromFoldable allTargets

-- | Convert Map Int Position to Object (PositionMap for simulation)
mapToPositionObject :: Map Int TargetPosition -> PositionMap
mapToPositionObject m =
  Object.fromFoldable $ map (\(Tuple k v) -> Tuple (show k) v) (Map.toUnfoldable m :: Array _)

-- | Ease-in-out cubic for smooth animations
easeInOutCubic :: Number -> Number
easeInOutCubic t =
  if t < 0.5
    then 4.0 * t * t * t
    else 1.0 - ((-2.0 * t + 2.0) `pow` 3.0) / 2.0
  where
    pow x _ = x * x * x  -- Simplified for cubic

-- | All categories visible by default
allCategories :: Set Int
allCategories = Set.fromFoldable [0, 1, 2, 3]

-- =============================================================================
-- Component State
-- =============================================================================

-- | Component state - V2 version
-- |
-- | Key changes from ForcePlayground:
-- | - simHandle instead of simulation (SimulationHandle vs raw D3 sim)
-- | - currentSetup tracks the Setup for force toggling
type State =
  { model :: Maybe NetworkModel
  , simHandle :: Maybe NetworkSimulationHandle
  , currentSetup :: Setup (SimulationNode Simple.NetworkNodeRow)
  , enabledForces :: Set ForceId
  , activePreset :: Maybe ForcePreset
  , shownCategories :: Set Int
  , visibleNodeIds :: Set Int
  , enteringProgress :: Map Int Tick.Progress
  , exitingNodes :: Array ExitingNode
  , quadrantTargets :: Map Int TargetPosition  -- Target positions for quadrant animation
  , quadrantStarts :: Map Int TargetPosition   -- Start positions captured when animation begins
  , quadrantProgress :: Number                 -- 0.0 to 1.0 animation progress
  }

-- | Component actions
data Action
  = Initialize
  | Finalize
  | SimTick Number          -- Simulation tick event (alpha value)
  | SimCompleted            -- Simulation converged
  | RegenerateGraph
  | ToggleForce ForceId
  | ApplyPreset ForcePreset
  | ToggleCategory Int
  | ShowAllCategories

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { model: Nothing
      , simHandle: Nothing
      , currentSetup: Simple.initialSetup
      , enabledForces: Simple.allForces
      , activePreset: Just PresetStandard
      , shownCategories: allCategories
      , visibleNodeIds: Set.empty
      , enteringProgress: Map.empty
      , exitingNodes: []
      , quadrantTargets: Map.empty
      , quadrantStarts: Map.empty
      , quadrantProgress: 1.0  -- Start at 1.0 (complete, no animation)
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall w. State -> HH.HTML w Action
render state =
  HH.div
    [ HP.classes [ HH.ClassName "force-playground-page" ] ]
    [ -- Header
      HH.header
        [ HP.classes [ HH.ClassName "demo-header" ] ]
        [ HH.h1_ [ HH.text "Force Playground" ]
        , HH.p_ [ HH.text "Interactive force-directed graph simulation demo" ]
        ]

    -- Fullscreen visualization container
    , HH.div
        [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "force-playground-viz" ] ]
        [ -- Main viz area
          HH.div
            [ HP.id "force-playground-container"
            , HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "svg-container" ]
            ]
            []

        -- Floating control panel (top-right)
        , renderControlPanel state
        ]
    ]

-- | Floating control panel - V2 version
renderControlPanel :: forall w. State -> HH.HTML w Action
renderControlPanel state =
  HH.div
    [ HP.classes [ HH.ClassName "semiotic-panel" ] ]
    [ -- Category Icons (top row)
      HH.div
        [ HP.classes [ HH.ClassName "semiotic-row" ] ]
        [ categoryIcon state 0 "#1e5a8c" iconCircle "RESEARCH"
        , categoryIcon state 1 "#d4a024" iconTriangle "INDUSTRY"
        , categoryIcon state 2 "#c41e3a" iconBars "GOVERNMENT"
        , categoryIcon state 3 "#2d8b57" iconDots "COMMUNITY"
        ]

    -- Force Icons (second row)
    , HH.div
        [ HP.classes [ HH.ClassName "semiotic-row" ] ]
        [ forceIcon state ForceCharge iconStarburst "CHARGE"
        , forceIcon state ForceCollide iconOverlap "COLLIDE"
        , forceIcon state ForceLink iconConnected "LINKS"
        , forceIcon state ForceX iconHorizontal "CENTER-X"
        , forceIcon state ForceY iconVertical "CENTER-Y"
        ]

    -- Preset Icons (third row)
    , HH.div
        [ HP.classes [ HH.ClassName "semiotic-row" ] ]
        [ presetIcon state PresetStandard iconClustered "CENTERED"
        , presetIcon state PresetClustered iconScattered "FLOATING"
        , presetIcon state PresetQuadrants iconQuadrants "QUADRANTS"
        ]

    -- Regenerate Icon (bottom)
    , HH.div
        [ HP.classes [ HH.ClassName "semiotic-row", HH.ClassName "semiotic-row--footer" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "semiotic-button", HH.ClassName "semiotic-button--wide" ]
            , HP.title "Generate new network"
            , HE.onClick \_ -> RegenerateGraph
            ]
            [ iconRefresh, HH.span [ HP.classes [ HH.ClassName "semiotic-label" ] ] [ HH.text "REGENERATE" ] ]
        ]

    -- V2 indicator
    , HH.div
        [ HP.classes [ HH.ClassName "semiotic-row" ] ]
        [ HH.span
            [ HP.classes [ HH.ClassName "v2-indicator" ]
            , HP.style "font-size: 10px; color: #2d8b57; opacity: 0.7;"
            ]
            [ HH.text "V2: runSimulation API" ]
        ]
    ]

-- =============================================================================
-- Icon Components (copied from ForcePlayground.purs)
-- =============================================================================

categoryIcon :: forall w. State -> Int -> String -> HH.HTML w Action -> String -> HH.HTML w Action
categoryIcon state catId color icon tooltip =
  let isVisible = Set.member catId state.shownCategories
  in HH.button
      [ HP.classes $
          [ HH.ClassName "semiotic-button" ] <>
          if isVisible then [ HH.ClassName "semiotic-button--active" ] else []
      , HP.style $ "--semiotic-color: " <> color <> ";"
      , HP.title tooltip
      , HE.onClick \_ -> ToggleCategory catId
      ]
      [ icon ]

forceIcon :: forall w. State -> ForceId -> HH.HTML w Action -> String -> HH.HTML w Action
forceIcon state forceId icon tooltip =
  let isEnabled = Set.member forceId state.enabledForces
  in HH.button
      [ HP.classes $
          [ HH.ClassName "semiotic-button" ] <>
          if isEnabled then [ HH.ClassName "semiotic-button--active" ] else []
      , HP.title tooltip
      , HE.onClick \_ -> ToggleForce forceId
      ]
      [ icon ]

presetIcon :: forall w. State -> ForcePreset -> HH.HTML w Action -> String -> HH.HTML w Action
presetIcon state preset icon tooltip =
  let isActive = state.activePreset == Just preset
  in HH.button
      [ HP.classes $
          [ HH.ClassName "semiotic-button" ] <>
          if isActive then [ HH.ClassName "semiotic-button--active" ] else []
      , HP.title tooltip
      , HE.onClick \_ -> ApplyPreset preset
      ]
      [ icon ]

-- SVG Icons (simplified versions - copied from ForcePlayground)
iconCircle :: forall w i. HH.HTML w i
iconCircle = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.circle [ SA.cx 20.0, SA.cy 20.0, SA.r 12.0, SA.classes [ HH.ClassName "semiotic-shape" ] ] ]

iconTriangle :: forall w i. HH.HTML w i
iconTriangle = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.polygon [ SA.points [ Tuple 20.0 6.0, Tuple 34.0 34.0, Tuple 6.0 34.0 ], SA.classes [ HH.ClassName "semiotic-shape" ] ] ]

iconBars :: forall w i. HH.HTML w i
iconBars = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.rect [ SA.x 10.0, SA.y 8.0, SA.width 6.0, SA.height 24.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.rect [ SA.x 24.0, SA.y 8.0, SA.width 6.0, SA.height 24.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

iconDots :: forall w i. HH.HTML w i
iconDots = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.circle [ SA.cx 13.0, SA.cy 13.0, SA.r 5.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 27.0, SA.cy 13.0, SA.r 5.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 13.0, SA.cy 27.0, SA.r 5.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 27.0, SA.cy 27.0, SA.r 5.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

iconStarburst :: forall w i. HH.HTML w i
iconStarburst = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.circle [ SA.cx 20.0, SA.cy 20.0, SA.r 4.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.line [ SA.x1 20.0, SA.y1 4.0, SA.x2 20.0, SA.y2 12.0, SA.classes [ HH.ClassName "semiotic-line" ] ]
  , SE.line [ SA.x1 20.0, SA.y1 28.0, SA.x2 20.0, SA.y2 36.0, SA.classes [ HH.ClassName "semiotic-line" ] ]
  , SE.line [ SA.x1 4.0, SA.y1 20.0, SA.x2 12.0, SA.y2 20.0, SA.classes [ HH.ClassName "semiotic-line" ] ]
  , SE.line [ SA.x1 28.0, SA.y1 20.0, SA.x2 36.0, SA.y2 20.0, SA.classes [ HH.ClassName "semiotic-line" ] ]
  ]

iconOverlap :: forall w i. HH.HTML w i
iconOverlap = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.circle [ SA.cx 15.0, SA.cy 20.0, SA.r 10.0, SA.classes [ HH.ClassName "semiotic-shape-stroke" ] ]
  , SE.circle [ SA.cx 25.0, SA.cy 20.0, SA.r 10.0, SA.classes [ HH.ClassName "semiotic-shape-stroke" ] ]
  ]

iconConnected :: forall w i. HH.HTML w i
iconConnected = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.line [ SA.x1 10.0, SA.y1 20.0, SA.x2 30.0, SA.y2 20.0, SA.classes [ HH.ClassName "semiotic-line" ] ]
  , SE.circle [ SA.cx 10.0, SA.cy 20.0, SA.r 5.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 30.0, SA.cy 20.0, SA.r 5.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

iconHorizontal :: forall w i. HH.HTML w i
iconHorizontal = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.line [ SA.x1 4.0, SA.y1 20.0, SA.x2 36.0, SA.y2 20.0, SA.classes [ HH.ClassName "semiotic-line" ] ]
  , SE.polygon [ SA.points [ Tuple 12.0 14.0, Tuple 4.0 20.0, Tuple 12.0 26.0 ], SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.polygon [ SA.points [ Tuple 28.0 14.0, Tuple 36.0 20.0, Tuple 28.0 26.0 ], SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 20.0, SA.cy 20.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

iconVertical :: forall w i. HH.HTML w i
iconVertical = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.line [ SA.x1 20.0, SA.y1 4.0, SA.x2 20.0, SA.y2 36.0, SA.classes [ HH.ClassName "semiotic-line" ] ]
  , SE.polygon [ SA.points [ Tuple 14.0 12.0, Tuple 20.0 4.0, Tuple 26.0 12.0 ], SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.polygon [ SA.points [ Tuple 14.0 28.0, Tuple 20.0 36.0, Tuple 26.0 28.0 ], SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 20.0, SA.cy 20.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

iconClustered :: forall w i. HH.HTML w i
iconClustered = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.circle [ SA.cx 20.0, SA.cy 20.0, SA.r 4.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 14.0, SA.cy 16.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 26.0, SA.cy 17.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 15.0, SA.cy 25.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 25.0, SA.cy 24.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

iconScattered :: forall w i. HH.HTML w i
iconScattered = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0, SA.classes [ HH.ClassName "semiotic-icon" ] ]
  [ SE.circle [ SA.cx 8.0, SA.cy 10.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 32.0, SA.cy 8.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 20.0, SA.cy 20.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 6.0, SA.cy 32.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 34.0, SA.cy 30.0, SA.r 3.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

-- | Quadrant grid with dots (Quadrants preset)
iconQuadrants :: forall w i. HH.HTML w i
iconQuadrants = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0
  , SA.classes [ HH.ClassName "semiotic-icon" ]
  ]
  [ -- Grid lines
    SE.line [ SA.x1 20.0, SA.y1 4.0, SA.x2 20.0, SA.y2 36.0, SA.classes [ HH.ClassName "semiotic-line-thin" ] ]
  , SE.line [ SA.x1 4.0, SA.y1 20.0, SA.x2 36.0, SA.y2 20.0, SA.classes [ HH.ClassName "semiotic-line-thin" ] ]
  -- Dots in each quadrant
  , SE.circle [ SA.cx 10.0, SA.cy 10.0, SA.r 4.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 30.0, SA.cy 10.0, SA.r 4.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 10.0, SA.cy 30.0, SA.r 4.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  , SE.circle [ SA.cx 30.0, SA.cy 30.0, SA.r 4.0, SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

iconRefresh :: forall w i. HH.HTML w i
iconRefresh = SE.svg
  [ SA.viewBox 0.0 0.0 40.0 40.0
  , SA.classes [ HH.ClassName "semiotic-icon", HH.ClassName "semiotic-icon--inline" ]
  ]
  [ SE.path [ SA.d [ SP.m SP.Abs 20.0 8.0, SP.a SP.Abs 12.0 12.0 0.0 SP.Arc1 SP.Sweep1 8.0 20.0 ], SA.classes [ HH.ClassName "semiotic-arc" ] ]
  , SE.polygon [ SA.points [ Tuple 4.0 12.0, Tuple 8.0 20.0, Tuple 12.0 12.0 ], SA.classes [ HH.ClassName "semiotic-shape" ] ]
  ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction RegenerateGraph

  Finalize -> do
    liftEffect Simple.unregisterSimulation

  SimTick _alpha -> do
    state <- H.get
    case state.simHandle, state.model of
      Just handle, Just model -> do
        -- Advance entering progress (0.0 -> 1.0)
        let delta = Simple.transitionDelta
            { active: stillEntering } = Tick.tickProgressMap delta state.enteringProgress

        -- Advance exiting transitions (0.0 -> 1.0)
        let { active: stillExiting } = Tick.tickTransitions delta state.exitingNodes

        -- Advance quadrant animation if in progress
        let newQuadrantProgress = min 1.0 (state.quadrantProgress + delta)
            isAnimatingQuadrants = state.quadrantProgress < 1.0 && not (Map.isEmpty state.quadrantTargets)

        -- If animating to quadrants, interpolate node positions toward targets
        when isAnimatingQuadrants $ liftEffect do
          -- Use easeInOutCubic for smooth animation
          let t = easeInOutCubic newQuadrantProgress
              startObj = mapToPositionObject state.quadrantStarts
              targetObj = mapToPositionObject state.quadrantTargets
          handle.interpolatePositions startObj targetObj t

        -- Update state with advanced progress
        H.modify_ _
          { enteringProgress = stillEntering
          , exitingNodes = stillExiting
          , quadrantProgress = newQuadrantProgress
          }

        -- Get current node positions from simulation handle
        currentNodes <- liftEffect handle.getNodes

        -- Build scene and render
        liftEffect do
          let visibleIds = Array.fromFoldable state.visibleNodeIds
              scene = buildSceneData
                currentNodes
                visibleIds
                stillEntering
                stillExiting
                model.links
          renderScene scene

      _, _ -> pure unit

  SimCompleted -> do
    liftEffect $ Console.log "V2: Simulation converged"

  RegenerateGraph -> do
    liftEffect $ clearContainer "#force-playground-container"

    -- Generate new network
    liftEffect $ Console.log "V2: Generating random network..."
    generated <- liftEffect $ Gen.generateGraph Gen.defaultConfig
    let model = fromGeneratedGraph generated
    liftEffect $ Console.log $ "V2: Generated: " <> show (length model.nodes) <> " nodes, " <> show (length model.links) <> " links"

    -- Create V2 simulation using runSimulation API
    -- KEY DIFFERENCE: Uses runSimulation instead of createSimulationWithCallbacks
    result <- liftEffect $ Simple.createSimulation model

    -- Register simulation for drag behavior
    -- KEY DIFFERENCE: Uses handle.reheat instead of Sim.reheat
    liftEffect $ Simple.registerSimulation result.handle.reheat

    -- Render SVG container
    liftEffect $ renderSVGContainer "#force-playground-container"

    -- Subscribe to simulation events via toHalogenEmitter
    -- KEY DIFFERENCE: Uses toHalogenEmitter instead of subscribeToSimulation
    halogenEmitter <- liftEffect $ toHalogenEmitter result.events
    void $ H.subscribe $ halogenEmitter <#> \event -> case event of
      Tick { alpha } -> SimTick alpha
      Completed -> SimCompleted
      Started -> SimTick 1.0  -- Treat start as a tick
      Stopped -> SimCompleted

    -- All nodes visible initially
    let allNodeIds = Set.fromFoldable $ map _.id model.nodes

    H.modify_ _
      { model = Just model
      , simHandle = Just result.handle
      , currentSetup = result.initialSetup
      , enabledForces = Simple.allForces
      , activePreset = Just PresetStandard
      , shownCategories = allCategories
      , visibleNodeIds = allNodeIds
      , enteringProgress = Map.empty
      , exitingNodes = []
      , quadrantTargets = Map.empty
      , quadrantStarts = Map.empty
      , quadrantProgress = 1.0
      }

    -- Do initial render
    doRender

  ToggleForce forceId -> do
    state <- H.get
    case state.simHandle of
      Nothing -> pure unit
      Just handle -> do
        let isEnabled = Set.member forceId state.enabledForces
            newSetup = if isEnabled
              then Simple.removeForceFromSetup forceId state.currentSetup
              else Simple.addForceToSetup forceId state.currentSetup
            newEnabledForces = if isEnabled
              then Set.delete forceId state.enabledForces
              else Set.insert forceId state.enabledForces

        -- KEY DIFFERENCE: Use handle.updateSetup instead of Sim.addForce/removeForce
        liftEffect $ handle.updateSetup newSetup

        H.modify_ _
          { currentSetup = newSetup
          , enabledForces = newEnabledForces
          , activePreset = Nothing  -- Custom config, no preset
          }

  ApplyPreset preset -> do
    state <- H.get
    case state.simHandle, state.model of
      Nothing, _ -> pure unit
      _, Nothing -> pure unit
      Just handle, Just model -> do
        let targetForces = presetForces preset

        -- Build new setup: start with no forces, add only target forces
        let emptySetup = foldl
              (\s fid -> Simple.removeForceFromSetup fid s)
              Simple.initialSetup
              (Set.toUnfoldable Simple.allForces :: Array ForceId)

            finalSetup = foldl
              (\s fid -> Simple.addForceToSetup fid s)
              emptySetup
              (Set.toUnfoldable targetForces :: Array ForceId)

        -- Use handle.updateSetup instead of individual addForce/removeForce calls
        liftEffect $ handle.updateSetup finalSetup

        -- If quadrants preset, set up animation
        case preset of
          PresetQuadrants -> do
            -- Capture current positions as start
            currentNodes <- liftEffect handle.getNodes
            let startPositions = Map.fromFoldable $
                  map (\n -> Tuple n.id { x: n.x, y: n.y }) currentNodes
                -- Compute target positions using phyllotaxis
                targetPositions = computeQuadrantTargets model.nodes

            H.modify_ _
              { currentSetup = finalSetup
              , enabledForces = targetForces
              , activePreset = Just preset
              , quadrantStarts = startPositions
              , quadrantTargets = targetPositions
              , quadrantProgress = 0.0  -- Start animation
              }

          _ -> do
            -- For other presets, clear quadrant animation state
            H.modify_ _
              { currentSetup = finalSetup
              , enabledForces = targetForces
              , activePreset = Just preset
              , quadrantTargets = Map.empty
              , quadrantStarts = Map.empty
              , quadrantProgress = 1.0
              }
    where
      foldl = Array.foldl

  ToggleCategory catId -> do
    state <- H.get
    case state.simHandle, state.model of
      Just handle, Just model -> do
        let isCurrentlyShown = Set.member catId state.shownCategories
            newShownCategories = if isCurrentlyShown
              then Set.delete catId state.shownCategories
              else Set.insert catId state.shownCategories

        -- Calculate new visible node IDs based on category filter
        let newVisibleIds = Set.fromFoldable $ map _.id $
              filter (\n -> Set.member n.group newShownCategories) model.nodes

        -- Find nodes being removed (start exit transitions)
        let removedIds = Set.difference state.visibleNodeIds newVisibleIds

        -- Get current positions for exiting nodes
        currentNodes <- liftEffect handle.getNodes
        let removedNodes = filter (\n -> Set.member n.id removedIds) currentNodes
            newExiting = Tick.startTransitions removedNodes

        -- Find nodes being added (start enter transitions)
        let addedIds = Set.difference newVisibleIds state.visibleNodeIds
            newEntering = Tick.startProgress (Array.fromFoldable addedIds) state.enteringProgress

        liftEffect $ handle.reheat

        H.modify_ _
          { shownCategories = newShownCategories
          , visibleNodeIds = newVisibleIds
          , enteringProgress = newEntering
          , exitingNodes = state.exitingNodes <> newExiting
          }

      _, _ -> pure unit

  ShowAllCategories -> do
    state <- H.get
    case state.simHandle, state.model of
      Just handle, Just model -> do
        let allNodeIds = Set.fromFoldable $ map _.id model.nodes
            addedIds = Set.difference allNodeIds state.visibleNodeIds
            newEntering = Tick.startProgress (Array.fromFoldable addedIds) state.enteringProgress

        liftEffect $ handle.reheat

        H.modify_ _
          { shownCategories = allCategories
          , visibleNodeIds = allNodeIds
          , enteringProgress = newEntering
          }

      _, _ -> pure unit

-- =============================================================================
-- Rendering Helpers
-- =============================================================================

doRender :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
doRender = do
  state <- H.get
  case state.simHandle, state.model of
    Just handle, Just model -> liftEffect do
      currentNodes <- handle.getNodes
      let visibleIds = Array.fromFoldable state.visibleNodeIds
          scene = buildSceneData
            currentNodes
            visibleIds
            state.enteringProgress
            state.exitingNodes
            model.links
      renderScene scene

    _, _ -> pure unit
