# purescript-hylograph-simulation

Force-directed graph simulation: the library's own simulation loop over a replaceable force kernel (`d3-force`'s force functions by default, or Rust/WASM).

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
    { engine: D3
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

## What comes from D3, and what does not

The force *calculations* are D3's. `ForceEngine/Core.js` imports the force
functions from `d3-force` (`forceManyBody`, `forceCollide`, `forceLink`,
`forceCenter`, `forceX`, `forceY`, `forceRadial`), plus `select` and `pointer`
from `d3-selection` and `drag` from `d3-drag` for dragging nodes.

The *simulation* is not D3's. There is no `d3.forceSimulation`, no D3 timer and
no D3 lifecycle: the tick loop, alpha decay, velocity integration and ownership
of the node array are this library's. That is what makes the general update
pattern work on a running simulation (`handle.updateData` adds and removes
nodes and links and reheats it), which D3's own simulation wrapper makes hard
because it owns the data.

A second kernel, written in Rust and compiled to WebAssembly, sits behind the
same `Engine` switch (`engine: WASM`, after `initWASM` has loaded the module;
bindings in `Hylograph.ForceEngine.WASM`) and uses no D3 force code. It is
there to show the kernel is replaceable; it is less exercised than the D3 one.

The same two kernels are also packaged separately, as
[hylograph-d3-kernel](https://github.com/afcondon/purescript-hylograph-d3-kernel)
and [hylograph-wasm-kernel](https://github.com/afcondon/purescript-hylograph-wasm-kernel)
over `hylograph-simulation-core`. This package does not depend on them; it
carries its own copy of both sets of bindings.

### npm dependencies

The PureScript registry cannot declare npm dependencies, so a project that
bundles this library has to install them itself:

```bash
npm install d3-force d3-selection d3-drag
```

## Features

- **Replaceable kernel** - Same API over `d3-force`'s force functions or a Rust/WASM kernel
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

## Modules

- `Hylograph.Simulation` - High-level API (recommended)
- `Hylograph.Simulation.Emitter` - Framework-agnostic event system
- `Hylograph.ForceEngine.Setup` - Declarative force configuration
- `Hylograph.ForceEngine.Simulation` - Low-level simulation control (the library's own loop)
- `Hylograph.ForceEngine.WASM` - WASM engine integration

## Part of Hylograph

This package is part of the Hylograph visualization ecosystem:
- **hylograph-selection** - HATS trees and DOM rendering
- **hylograph-simulation** - Force simulation (this package)
- **hylograph-simulation-halogen** - Halogen integration
- **hylograph-graph** - Graph data structures
- **hylograph-layout** - Layout algorithms

## License

MIT
