-- | One simulation, stepped by its caller, on either engine.
-- |
-- | `runSimulation` owns the clock: it ticks on animation frames and tells
-- | you. This is for when you own it: a timeline that can be scrubbed, a
-- | layout computed before anything is drawn, a test. `create` takes the
-- | engine, and everything after that is the same record of functions
-- | whichever engine is behind it, meaning the same thing:
-- |
-- | - `step` makes one tick and returns the nodes where it left them;
-- |   `advance` makes several and returns them once;
-- | - `settled` and the three measures say whether it has come to rest;
-- | - `snapshot` and `restore` go back to a moment exactly: the ticks that
-- |   follow a restore are the ticks that followed the snapshot. A snapshot
-- |   carries the tick number, the nodes, the links and the setup, so a
-- |   restore needs nothing else, even after the nodes, links or forces have
-- |   been changed;
-- | - `retarget` gives a Towards force new targets and `reradius` gives a
-- |   Collide, Radial or Contain force new radii, without applying the
-- |   setup again; `reheat` puts alpha back to one; `pin` holds or lets go
-- |   of one node; `relink` puts other links between the same nodes;
-- | - `applySetup` changes the forces and parameters and keeps the nodes
-- |   moving.
-- |
-- | THE NODES ARE VALUES. `create` copies the records it is given and never
-- | writes into them. `step`, `nodes` and `restore` return records that are
-- | new after every tick, on both engines. One thing may be written on the
-- | records most recently returned: a pin (`fx`, `fy`, as a drag does),
-- | which the next `step` reads. (The engines underneath differ here, and
-- | this module is where they are made the same: the default engine,
-- | `Hylograph.ForceEngine.Simulation`, adopts the records it is given and
-- | moves them in place, as d3 does; the Rust one,
-- | `Hylograph.ForceEngine.Setup.WASM`, keeps its numbers in the kernel and
-- | makes new records each tick.)
-- |
-- | ALPHA is zero once it has fallen below the setup's `alphaMin`, on both
-- | engines (it is the default engine's rule; the kernel alone would go on
-- | cooling), so a force scaled by alpha has then stopped altogether.
-- |
-- | The Rust engine needs `initWasm` (or `installKernel`) first, and `free` when done with. The
-- | two engines compute the same rules, the default one in f64 and the Rust
-- | one in f32, so they agree to float width over a tick and drift apart
-- | after that, as any two roundings of a force layout do.
module Hylograph.Simulation.Stepped
  ( Engine(..)
  , Stepped
  , Snapshot
  , Tolerance
  , Link
  , Point
  , create
  , initWasm
  , installKernel
  , module KernelModuleExport
  , snapshotTick
  , snapshotNodes
  , snapshotLinks
  , snapshotAlpha
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.ForceEngine.Setup (ForceConfig(..), Setup)
import Hylograph.ForceEngine.Setup as Setup
import Hylograph.ForceEngine.Setup.WASM as Wasm
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.ForceEngine.Simulation as Default
import Hylograph.ForceEngine.WASM as Kernel
import Hylograph.ForceEngine.WASM (KernelModule)
import Hylograph.ForceEngine.WASM (KernelModule) as KernelModuleExport
import Hylograph.Forces.Measure as Measure
import Hylograph.Forces.Node (writePin)
import Hylograph.Forces.Snapshot as Kept

-- | Which engine runs the simulation: the default one (the pure PureScript
-- | kernel, f64, nothing to load) or the Rust kernel (WASM, f32, faster).
data Engine
  = Default
  | Rust

derive instance Eq Engine

-- | How still is at rest: no node moved further than `step` in a tick, and
-- | no node's step changed by more than `acceleration`, over the last two.
type Tolerance = { step :: Number, acceleration :: Number }

type Link = { source :: Int, target :: Int }

type Point = { x :: Number, y :: Number }

-- | A simulation under its caller's control. The same on both engines.
type Stepped row =
  { engine :: Engine
  -- | One tick; the nodes where it left them.
  , step :: Effect (Array (SimulationNode row))
  -- | This many ticks, exactly as that many calls of `step` would make
  -- | them; the nodes where the last one left them. None, if not positive.
  , advance :: Int -> Effect (Array (SimulationNode row))
  -- | The nodes as the last tick (or `create`, or `restore`) left them.
  , nodes :: Effect (Array (SimulationNode row))
  , links :: Effect (Array Link)
  , alpha :: Effect Number
  -- | How many ticks have been made (a restore puts this back too).
  , tick :: Effect Int
  -- | Has alpha fallen below the setup's `alphaMin`?
  , cooled :: Effect Boolean
  -- | At rest, by what the nodes actually did? False until two ticks have
  -- | been made since `create` or a `restore`.
  , settled :: Tolerance -> Effect Boolean
  , kineticEnergy :: Effect Number
  -- | The furthest any node moved in the last tick.
  , maxDisplacement :: Effect Number
  -- | The largest change in any node's step between the last two ticks.
  , maxAcceleration :: Effect Number
  -- | Other forces and parameters (every one of them: alpha, alphaMin,
  -- | alphaDecay, alphaTarget, velocityRetention), same nodes, still moving.
  -- | Throws, changing nothing, if a force along the links meets a link to
  -- | a node that is not there.
  , applySetup :: Setup (SimulationNode row) -> Effect Unit
  -- | New targets for the Towards force of this name. False if the setup
  -- | has none.
  , retarget :: String -> (SimulationNode row -> Maybe Point) -> Effect Boolean
  -- | New radii for the Collide, Radial or Contain force of this name.
  -- | False if the setup has none.
  , reradius :: String -> (SimulationNode row -> Number) -> Effect Boolean
  -- | Other links between the same nodes, which stay where they are and
  -- | keep moving; alpha is what it was. Throws, changing nothing, if the
  -- | setup has a force along the links and a link names a node that is
  -- | not there.
  , relink :: Array Link -> Effect Unit
  -- | Alpha back to one, so that forces scaled by it push again. Changes
  -- | nothing else: not the forces, the nodes or the tick count.
  , reheat :: Effect Unit
  -- | Hold the node with this id at a point (`Just`), or let it go.
  , pin :: Int -> Maybe Point -> Effect Unit
  , snapshot :: Effect (Snapshot row)
  -- | Back to a snapshot; the nodes as they then were.
  , restore :: Snapshot row -> Effect (Array (SimulationNode row))
  -- | Give back what the engine holds. Use nothing of the handle after.
  , free :: Effect Unit
  }

-- | A moment of a simulation, complete: restoring it needs nothing else.
newtype Snapshot row = Snapshot
  { tick :: Int
  , alpha :: Number
  , nodes :: Array (SimulationNode row)
  , links :: Array Link
  , setup :: Setup (SimulationNode row)
  , state :: State
  }

-- | What an engine keeps beyond the nodes: the stream that separates
-- | coincident nodes, and the numbers exactly as the engine holds them.
data State
  = DefaultState Kept.Snapshot
  | RustState Kernel.Snapshot

-- | The tick the snapshot was taken after.
snapshotTick :: forall row. Snapshot row -> Int
snapshotTick (Snapshot recorded) = recorded.tick

snapshotNodes :: forall row. Snapshot row -> Array (SimulationNode row)
snapshotNodes (Snapshot recorded) = recorded.nodes

snapshotLinks :: forall row. Snapshot row -> Array { source :: Int, target :: Int }
snapshotLinks (Snapshot recorded) = recorded.links

snapshotAlpha :: forall row. Snapshot row -> Number
snapshotAlpha (Snapshot recorded) = recorded.alpha

-- | Load the Rust kernel (once, before `create Rust`).
initWasm :: String -> Aff Unit
initWasm = Wasm.initWasm

-- | Install a kernel module loaded by other means instead (once, before
-- | `create Rust`); `hylograph-wasm-kernel` makes one from bytes it carries.
installKernel :: KernelModule -> Effect Unit
installKernel = Wasm.installKernel

-- | A simulation of these nodes and links under this setup. The nodes are
-- | copied; links name their ends by node `id`. Throws if the setup has a
-- | force along the links and a link names a node that is not there.
create
  :: forall row
   . Engine
  -> Setup (SimulationNode row)
  -> Array (SimulationNode row)
  -> Array { source :: Int, target :: Int }
  -> Effect (Stepped row)
create engine setup nodes links = case engine of
  Default -> createDefault setup nodes links
  Rust -> createRust setup nodes links

-- | A new record with the same fields. (A record update copies.)
copy :: forall row. SimulationNode row -> SimulationNode row
copy node = node { x = node.x }

-- | The force of that name, with new targets if it is a Towards force.
retargeted
  :: forall node
   . String
  -> (node -> Maybe Point)
  -> Setup node
  -> Maybe { force :: ForceConfig node, setup :: Setup node }
retargeted name target setup =
  Array.findMap towards setup.forces <#> \force -> { force, setup: Setup.updateForce force setup }
  where
  towards = case _ of
    TowardsForce config | config.name == name -> Just (TowardsForce config { target = target })
    _ -> Nothing

-- | The force of that name, with new radii if it is one that has radii.
reradiused
  :: forall node
   . String
  -> (node -> Number)
  -> Setup node
  -> Maybe { force :: ForceConfig node, setup :: Setup node }
reradiused name radius setup =
  Array.findMap sized setup.forces <#> \force -> { force, setup: Setup.updateForce force setup }
  where
  sized force = case force of
    CollideForce config | config.name == name -> Just (Setup.withRadius (Setup.dynamic radius) force)
    RadialForce config | config.name == name -> Just (Setup.withRadius (Setup.dynamic radius) force)
    ContainForce config | config.name == name -> Just (Setup.withRadius (Setup.dynamic radius) force)
    _ -> Nothing

-- | Write a pin on the node with this id, where there is one.
pinById :: forall row. Int -> Maybe Point -> Array (SimulationNode row) -> Effect Unit
pinById id at nodes =
  for_ (Array.find (\node -> node.id == id) nodes) \node -> case at of
    Just point -> runEffectFn3 writePin node (notNull point.x) (notNull point.y)
    Nothing -> runEffectFn3 writePin node null null

-- | The pins of the records last handed out, onto the engine's own.
carryPins :: forall row. Array (SimulationNode row) -> Array (SimulationNode row) -> Effect Unit
carryPins handed own =
  when (Array.length handed == Array.length own) do
    for_ (Array.zip handed own) \(Tuple from to) ->
      runEffectFn3 writePin to from.fx from.fy

createDefault
  :: forall row
   . Setup (SimulationNode row)
  -> Array (SimulationNode row)
  -> Array Link
  -> Effect (Stepped row)
createDefault firstSetup firstNodes firstLinks = do
  let params = Setup.getParams firstSetup
  simulation <- Default.create
    { alphaMin: params.alphaMin
    , alphaDecay: params.alphaDecay
    , alphaTarget: params.alphaTarget
    , velocityRetention: params.velocityRetention
    }
  Default.setNodes (copy <$> firstNodes) simulation
  Default.setLinks firstLinks simulation
  Setup.applySetup firstSetup simulation
  watch <- Measure.watch
  current <- Ref.new firstSetup
  ticks <- Ref.new 0
  startingNodes <- Default.getNodes simulation
  handed <- Ref.new (copy <$> startingNodes)
  let
    handOut = do
      own <- Default.getNodes simulation
      let copies = copy <$> own
      Ref.write copies handed
      pure copies

    pinsIn = do
      own <- Default.getNodes simulation
      latest <- Ref.read handed
      carryPins latest own
      pure own

    -- One tick, measured and counted; nothing handed out.
    oneTick own = do
      Measure.before watch own
      void $ Default.tick simulation
      Measure.after watch own
      Ref.modify_ (_ + 1) ticks

  pure
    { engine: Default
    , step: do
        own <- pinsIn
        oneTick own
        handOut
    , advance: \count ->
        if count <= 0 then Ref.read handed
        else do
          own <- pinsIn
          for_ (Array.range 1 count) \_ -> oneTick own
          handOut
    , nodes: Ref.read handed
    , links: Default.getLinks simulation
    , alpha: Default.getAlpha simulation
    , tick: Ref.read ticks
    , cooled: do
        setup <- Ref.read current
        (_ < (Setup.getParams setup).alphaMin) <$> Default.getAlpha simulation
    , settled: \tolerance -> Measure.settled tolerance watch
    , kineticEnergy: Default.getNodes simulation >>= Measure.kineticEnergy
    , maxDisplacement: Measure.maxDisplacement watch
    , maxAcceleration: Measure.maxAcceleration watch
    , applySetup: \setup -> do
        void pinsIn
        Setup.applySetup setup simulation
        Ref.write setup current
    , retarget: \name target -> do
        setup <- Ref.read current
        case retargeted name target setup of
          Nothing -> pure false
          Just changed -> do
            Setup.addForceToSim changed.force simulation
            Ref.write changed.setup current
            pure true
    , reradius: \name radius -> do
        setup <- Ref.read current
        case reradiused name radius setup of
          Nothing -> pure false
          Just changed -> do
            Setup.addForceToSim changed.force simulation
            Ref.write changed.setup current
            pure true
    , relink: \links -> do
        own <- pinsIn
        setup <- Ref.read current
        case Setup.linkProblem setup own links of
          Just problem -> throw ("Hylograph.Simulation.Stepped: " <> problem <> ". Nothing was changed.")
          Nothing -> Default.setLinks links simulation
    , reheat: Ref.write 1.0 simulation.alpha
    , pin: \id at -> do
        Default.getNodes simulation >>= pinById id at
        Ref.read handed >>= pinById id at
    , snapshot: do
        own <- pinsIn
        now <- Default.getAlpha simulation
        state <- Kept.take simulation.random now own
        tick <- Ref.read ticks
        setup <- Ref.read current
        kept <- Default.getLinks simulation
        pure (Snapshot { tick, alpha: now, nodes: copy <$> own, links: kept, setup, state: DefaultState state })
    , restore: \(Snapshot recorded) -> case recorded.state of
        RustState _ -> throw "Hylograph.Simulation.Stepped: a snapshot of the Rust engine cannot be restored to the default engine."
        DefaultState state -> do
          Default.setNodes (copy <$> recorded.nodes) simulation
          Default.setLinks recorded.links simulation
          Setup.applySetup recorded.setup simulation
          Ref.write recorded.setup current
          own <- Default.getNodes simulation
          then_ <- Kept.restore state simulation.random own
          Ref.write then_ simulation.alpha
          Ref.write then_ simulation.prevAlpha
          Ref.write recorded.tick ticks
          Measure.forget watch
          handOut
    , free: pure unit
    }

createRust
  :: forall row
   . Setup (SimulationNode row)
  -> Array (SimulationNode row)
  -> Array Link
  -> Effect (Stepped row)
createRust firstSetup firstNodes firstLinks = do
  first <- Wasm.create (copy <$> firstNodes) firstLinks
  Wasm.applySetup firstSetup first
  held <- Ref.new first
  current <- Ref.new firstSetup
  kept <- Ref.new firstLinks
  ticks <- Ref.new 0
  let
    -- The engine's records are new after every tick, so the ones handed
    -- out are copies only where the caller might otherwise be handed the
    -- same array twice.
    handOut = Ref.read held >>= Wasm.getNodes

    -- One tick, counted. The default engine's alpha is zero once it has
    -- fallen below alphaMin; the kernel's goes on cooling. Here they are
    -- the same, after every tick, so that several ticks made at once are
    -- the ticks made one at a time.
    oneTick simulation = do
      after <- Wasm.step simulation
      Ref.modify_ (_ + 1) ticks
      setup <- Ref.read current
      cooledTo <- Wasm.getAlpha simulation
      when (cooledTo < (Setup.getParams setup).alphaMin && cooledTo /= 0.0) do
        Kernel.setAlpha simulation.wasmSim 0.0
      pure after

  pure
    { engine: Rust
    , step: Ref.read held >>= oneTick
    , advance: \count ->
        if count <= 0 then handOut
        else do
          simulation <- Ref.read held
          for_ (Array.range 1 count) \_ -> oneTick simulation
          handOut
    , nodes: handOut
    , links: Ref.read kept
    , alpha: Ref.read held >>= Wasm.getAlpha
    , tick: Ref.read ticks
    , cooled: do
        setup <- Ref.read current
        simulation <- Ref.read held
        (_ < (Setup.getParams setup).alphaMin) <$> Wasm.getAlpha simulation
    , settled: \tolerance -> Ref.read held >>= Wasm.settled tolerance
    , kineticEnergy: Ref.read held >>= Wasm.kineticEnergy
    , maxDisplacement: Ref.read held >>= Wasm.maxDisplacement
    , maxAcceleration: Ref.read held >>= Wasm.maxAcceleration
    , applySetup: \setup -> do
        simulation <- Ref.read held
        Wasm.applySetup setup simulation
        Ref.write setup current
    , retarget: \name target -> do
        setup <- Ref.read current
        case retargeted name target setup of
          Nothing -> pure false
          Just changed -> do
            Ref.write changed.setup current
            simulation <- Ref.read held
            Wasm.retarget name target simulation
    , reradius: \name radius -> do
        setup <- Ref.read current
        case reradiused name radius setup of
          Nothing -> pure false
          Just changed -> do
            Ref.write changed.setup current
            simulation <- Ref.read held
            Wasm.reradius name radius simulation
    , relink: \links -> do
        simulation <- Ref.read held
        setup <- Ref.read current
        own <- Wasm.getNodes simulation
        case Setup.linkProblem setup own links of
          Just problem -> throw ("Hylograph.Simulation.Stepped: " <> problem <> ". Nothing was changed.")
          Nothing -> do
            -- The kernel takes the links, and the forces along them are
            -- made again by applying the setup, which also sets alpha to
            -- the setup's: put back what it was.
            before <- Wasm.getAlpha simulation
            Wasm.relink links simulation
            Wasm.applySetup setup simulation
            Kernel.setAlpha simulation.wasmSim before
            Ref.write links kept
    , reheat: Ref.read held >>= Wasm.reheat
    , pin: \id at -> handOut >>= pinById id at
    , snapshot: do
        simulation <- Ref.read held
        state <- Wasm.snapshot simulation
        own <- Wasm.getNodes simulation
        now <- Wasm.getAlpha simulation
        tick <- Ref.read ticks
        setup <- Ref.read current
        links <- Ref.read kept
        pure (Snapshot { tick, alpha: now, nodes: copy <$> own, links, setup, state: RustState state })
    , restore: \(Snapshot recorded) -> case recorded.state of
        DefaultState _ -> throw "Hylograph.Simulation.Stepped: a snapshot of the default engine cannot be restored to the Rust engine."
        RustState state -> do
          -- A new kernel simulation of the snapshot's nodes, links and
          -- forces, and then the numbers exactly as they were.
          old <- Ref.read held
          Wasm.free old
          simulation <- Wasm.create (copy <$> recorded.nodes) recorded.links
          Wasm.applySetup recorded.setup simulation
          ok <- Wasm.restore state simulation
          unless ok (throw "Hylograph.Simulation.Stepped: the kernel refused the snapshot.")
          Ref.write simulation held
          Ref.write recorded.setup current
          Ref.write recorded.links kept
          Ref.write recorded.tick ticks
          Wasm.getNodes simulation
    , free: Ref.read held >>= Wasm.free
    }
