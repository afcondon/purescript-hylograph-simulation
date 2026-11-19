# purescript-hylograph-simulation

Force-directed graph simulation: the library's own simulation loop over a replaceable force kernel. The default kernel is PureScript ([hylograph-forces](https://github.com/afcondon/purescript-hylograph-forces): d3-force's forces, and six more); a Rust/WASM kernel is the fast path. There is no D3 in this library.

Live interactive demo can be seen as part of the [suite of Hylograph library demos](https://afcondon.github.io/purescript-hylograph-demos/). Click the screenshot below for the **Force Playground**.

[![Force Playground Demo](https://afcondon.github.io/purescript-hylograph-demos/thumbnails/forces.jpg)](https://afcondon.github.io/purescript-hylograph-demos/simulation/)

## Quick Start

```purescript
module Main where

import Prelude

import Data.Nullable (null)
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Simulation
  ( Engine(..), SimulationEvent(..), runSimulation, subscribe
  , setup, manyBody, center, withStrength, withX, withY, static
  )

main :: Effect Unit
main = do
  { handle, events } <- runSimulation
    { engine: Pure
    , setup: setup "physics"
        [ manyBody "charge" # withStrength (static (-50.0))
        , center "center" # withX (static 200.0) # withY (static 150.0)
        ]
    , nodes:
        [ { id: 0, x: 190.0, y: 140.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
        , { id: 1, x: 200.0, y: 150.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
        , { id: 2, x: 210.0, y: 160.0, vx: 0.0, vy: 0.0, fx: null, fy: null }
        ]
    , links: []
    , container: "#visualization"
    , alphaMin: 0.001
    }

  -- The library does not draw. On each tick, read the positions and
  -- render them however you like (a HATS tree, Halogen, canvas).
  _ <- subscribe events \event -> case event of
    Tick { alpha } -> do
      nodes <- handle.getNodes
      log $ show (map _.x nodes) <> " alpha " <> show alpha
    Completed -> log "Simulation converged"
    _ -> pure unit

  pure unit
```

## Installation

```bash
spago install hylograph-simulation
```

Its kernel, `hylograph-forces`, and `hylograph-selection` 0.6 were published
after package set 80.8.0 was cut. If your package set does not have them
yet, name them in your workspace:

```yaml
workspace:
  extraPackages:
    hylograph-simulation: "1.1.0"
    hylograph-forces: "0.1.0"
    hylograph-selection: "0.6.0"
```

**The WASM engine is optional and its module is not in this package.** The
default engine needs nothing loaded. To use `engine: WASM` (or
`Stepped.Rust`), build the module from its source,
[hylograph-force-kernel](https://github.com/afcondon/hylograph-force-kernel)
(`wasm-pack build --target web`; its README has the steps), serve the
resulting `pkg/` directory beside your page, and call `initWasm` with the URL
of `force_kernel.js` before creating a simulation.

Since 1.1.0 there is a second way in, `installKernel :: KernelModule ->
Effect Unit` (in `Hylograph.ForceEngine.WASM`, and re-exported by
`Hylograph.Simulation`, `Hylograph.Simulation.Stepped`,
`Hylograph.ForceEngine.Setup.WASM` and `Hylograph.ForceEngine.WASMEngine`).
It takes a module that something else has already loaded and instantiated,
and afterwards everything that works after `initWasm` works. The package
`hylograph-wasm-kernel` (1.0.0, not yet published) uses it to carry the module
inside a PureScript package, so that nothing has to be built or served:
`Hylograph.Kernel.WASM.install` decodes the bytes it carries and installs
them. This package does not depend on that one, so the default engine still
costs nothing. Whichever function runs first wins: once a module is
installed, `initWasm` and `installKernel` both do nothing.

## No D3

Until 2026-11-18 the force calculations here were d3-force's functions and
dragging went through d3-drag. Neither is used now, and the library has no
npm dependencies.

The forces are `hylograph-forces`: d3-force 3.0.0's seven forces ported
to PureScript and held to d3-force bit for bit by a differential test (same
nodes, same parameters, every value of every node compared after every tick),
plus the forces the Rust kernel added (towards, rectangle collision,
containment, cohesion, group-scoped many-body, directional links), held bit
for bit to that kernel built at 64 bits. See that package's README for the
tables and the timings.

The *simulation* was never D3's: the tick loop, alpha decay, velocity
integration and ownership of the node array are this library's. That is what
makes the general update pattern work on a running simulation
(`handle.updateData` adds and removes nodes and links and reheats it).

Dragging uses native Pointer Events: `attachDrag` is
`Hylograph.Interaction.Pointer.attachSimulationDrag` from hylograph-selection,
and the pinning and group drags are a small amount of this library's own
JavaScript.

Three things changed with the kernel:

- **Layouts repeat.** Coincident nodes are separated by a seeded generator,
  one per simulation, where the old engine used `Math.random`. The same
  nodes, links and setup give the same positions on every run.
- **A node with no position starts on d3's phyllotaxis spiral**, and a pinned
  node starts on its pin (d3's `initializeNodes`). The old engine put an
  unpositioned node somewhere at random.
- **A link naming a node that is not there is an error** (`node not found:
  <id>`, d3's words) when the setup has a force along the links:
  `applySetup` throws and `tryApplySetup` returns it as a `Left`, on both
  engines.

A second kernel, written in Rust and compiled to WebAssembly, sits behind the
same `Engine` switch (`engine: WASM`, after `initWASM` has loaded the module;
bindings in `Hylograph.ForceEngine.WASM`). One `Setup` drives both, every
force included, and `test/engine-parity.mjs` holds them to each other (to
float width one tick from identical state: the shipped kernel is f32).

The handle's calls do the same on both too (`test/browser-check.mjs` drives
each engine in headless Chrome): a drag after the simulation has completed,
`pinNodes`, `unpinNodes`, `interpolatePositions`. And a `Setup`'s params
(alpha, alphaDecay, alphaTarget, velocityRetention) are the simulation's on
both, whatever it was created with. What still differs: the default engine
moves the same node objects every tick, and the WASM engine hands out fresh
records after each tick (a pin set on the latest ones is honoured).

`engine: D3` still compiles and now means `Pure`.

### How fast

Many-body, links and centring through this library's own loop, milliseconds
per tick, Node 22 on an M1 Pro that was busy with other work (`node
test/timing.mjs`; each block of ten ticks counted at its fastest over three
interleaved runs):

| Nodes | default engine (PureScript, stock `purs`) | WASM engine |
|---|---|---|
| 100 | 0.19 | 0.04 |
| 1,000 | 2.9 | 0.87 |
| 10,000 | 46 | 12.6 |

The WASM engine is 3 to 4 times faster. The default engine is about where
d3-force's own functions are in the same loop (hylograph-forces's README
has that comparison, and the figures under `purs-backend-es`, which are
about a third lower). Not measured in a browser.

The D3 kernel that this library used to carry was packaged separately as
[hylograph-d3-kernel](https://github.com/afcondon/purescript-hylograph-d3-kernel),
which is now deprecated: this library replaces it. This package depends on
neither it nor `hylograph-wasm-kernel`.

## Features

- **Replaceable kernel** - Same API over a PureScript kernel or a Rust/WASM kernel
- **Repeatable** - The same nodes, links and setup give the same layout every time
- **Framework Agnostic** - Works with Halogen, React, or vanilla JS
- **Declarative Forces** - Compose forces with a fluent builder API
- **Event Subscription** - React to Tick, Started, Stopped, Completed events
- **GUP Semantics** - Enter/update/exit tracking when data changes, on a running simulation

## Halogen Integration

`toHalogenEmitter` is in the separate package `hylograph-simulation-halogen`.

```purescript
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)

handleAction Initialize = do
  { handle, events } <- liftEffect $ runSimulation config
  halogenEmitter <- liftEffect $ toHalogenEmitter events
  void $ H.subscribe $ halogenEmitter <#> SimEvent

handleAction (SimEvent (Tick { alpha })) =
  H.modify_ _ { alpha = alpha }
handleAction (SimEvent Completed) =
  liftEffect $ log "Done!"
```

## React Integration

```javascript
useEffect(() => {
  const { handle, events } = runSimulation(config)();
  const unsubscribe = subscribe(events)(event => {
    if (event.tag === 'Completed') {
      console.log('Simulation converged!');
    }
  })();
  return () => unsubscribe();
}, []);
```

## Available Forces

| Force | Description |
|-------|-------------|
| `manyBody` | N-body charge simulation (attract/repel) |
| `center` | Pulls nodes toward a center point |
| `link` | Spring forces between connected nodes |
| `collide` | Prevents node overlap |
| `positionX` | Pulls nodes toward an X position |
| `positionY` | Pulls nodes toward a Y position |
| `radial` | Pulls nodes toward a circle |
| `towards` | Pulls each node toward its own target point |
| `rectCollide` | Prevents rectangles (labels) overlapping |
| `contain` | Keeps nodes inside a rectangle, circle or convex polygon |
| `cohesion` | Pulls each node toward the centroid of its group |
| `directional` | Keeps each link's target a gap beyond its source along an axis |

`manyBody` can be scoped to pairs within a group or between groups, and
`collide` can take a filter and a strength that follows alpha.

`rectCollide` takes a filter (`withFilter`) and the same scoping by group
(`withinGroups`, `betweenGroups`: labels against dots, say, and not each
other). A pinned rectangle is never moved by it, and what overlaps a pinned
rectangle takes the whole push. With `withThroughSqueeze true`, a rectangle
pushed from both sides on an axis goes through one of them instead of
staying where the pushes cancel, which is what resolves crowded wide
rectangles (labels among dots); it wants nothing else pressing them
together, so let any pull cool to nothing. `contain` keeps a rectangle
inside by its half-extents (`withHalfWidth`, `withHalfHeight`) as well as a
circle by its radius. `directional` with `withHardLimit true` keeps exactly
the gap asked for: after each tick a link short of its gap has its target
moved along the axis by the shortfall. The rules in full are with the
kernels (`Hylograph.Forces.RectCollide`, `.Contain`, `.Directional`).

## Stepping it yourself: one handle, either engine

`runSimulation` owns the clock. When you do (a timeline to scrub, a layout
computed before anything is drawn, a test), use
`Hylograph.Simulation.Stepped`:

```purescript
import Hylograph.Simulation.Stepped as Stepped

sim <- Stepped.create Stepped.Default setup nodes links   -- or Stepped.Rust, after initWasm
nodes1 <- sim.step                       -- one tick; the nodes where it left them
still <- sim.settled { step: 0.05, acceleration: 0.05 }
moment <- sim.snapshot                   -- carries its tick, the nodes, the links, the setup
sim.pin 7 (Just { x: 100.0, y: 50.0 })   -- hold node 7; Nothing lets it go
_ <- sim.retarget "home" newTargets      -- a Towards force's targets
_ <- sim.reradius "bump" newRadius       -- a Collide, Radial or Contain force's radii
sim.relink otherLinks                    -- other links between the same nodes
sim.reheat                               -- alpha back to one
nodes10 <- sim.advance 10                -- ten ticks, exactly as ten steps
sim.applySetup otherSetup                -- other forces and parameters, still moving
nodes2 <- sim.restore moment             -- the ticks that follow are the ones that followed
```

The record is the same on both engines and means the same: `step`,
`advance`, `nodes`, `links`, `alpha`, `tick`, `cooled`, `settled`,
`kineticEnergy`, `maxDisplacement`, `maxAcceleration`, `applySetup`,
`retarget`, `reradius`, `relink`, `reheat`, `pin`, `snapshot`, `restore`,
`free`. `test/stepped.mjs` runs every one on both.

**Whose records are the nodes?** Through this handle they are values:
`create` copies what it is given and never writes into it, and every `step`
returns new records. A pin written on the records most recently returned
(`fx`, `fy`, as a drag does) is read by the next step. Underneath, the
engines differ, on purpose: `Hylograph.ForceEngine.Simulation` (the default
engine's own loop) adopts the records passed to `setNodes`, moves them in
place and returns the same records from `getNodes` every time, as d3 does,
which is what drag and a DOM bound to the nodes rely on;
`Hylograph.ForceEngine.Setup.WASM` keeps its numbers in the kernel and
returns new records after each tick. If you use those modules directly, that
is the difference to mind; `Stepped` is where they are made the same.

`applySetup` applies all of a setup's parameters on both engines (alpha,
alphaMin, alphaDecay, alphaTarget, velocityRetention), and through the handle
alpha is zero once it has cooled below `alphaMin` on both.

## Modules

- `Hylograph.Simulation` - High-level API (recommended)
- `Hylograph.Simulation.Stepped` - One caller-stepped handle over both engines: step, settle, snapshot, restore, pin
- `Hylograph.Simulation.Emitter` - Framework-agnostic event system
- `Hylograph.ForceEngine.Setup` - Declarative force configuration
- `Hylograph.ForceEngine.Simulation` - Low-level simulation control (the library's own loop)
- `Hylograph.ForceEngine.Core` - Force recipes and handles, the tick's parts, dragging
- `Hylograph.ForceEngine.WASM` - WASM engine integration

## Tests

```bash
spago test                      # the pure computations
node test/engine-parity.mjs     # one Setup through both engines, compared
node test/wasm-engine-api.mjs   # the WASM engine's PureScript API
node test/stepped.mjs           # the stepped handle, every function on both engines
node test/browser-check.mjs     # headless Chrome: the Force Playground demo, and the three drags
node test/timing.mjs            # ms per tick on each engine
```

The last five read the compiled `output/` and the WASM kernel: set
`KERNEL_PKG` to a `pkg/` directory built from
[hylograph-force-kernel](https://github.com/afcondon/hylograph-force-kernel)
(without it they look where the author's workspace keeps one). The browser
check also needs the bundled Force Playground from
[purescript-hylograph-demos](https://github.com/afcondon/purescript-hylograph-demos)
checked out beside this repository, and puppeteer-core.

## Part of Hylograph

This package is part of the Hylograph visualization ecosystem:
- **hylograph-selection** - HATS trees and DOM rendering
- **hylograph-simulation** - Force simulation (this package)
- **hylograph-simulation-halogen** - Halogen integration
- **hylograph-graph** - Graph data structures
- **hylograph-layout** - Layout algorithms

## License

MIT
