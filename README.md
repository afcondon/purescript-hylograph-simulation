# purescript-psd3-simulation

Force-directed graph simulation with unified D3 and WASM engine support.

## Quick Start

```purescript
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Nullable (null) as Nullable

import PSD3.Simulation
  ( runSimulation, Engine(..), SimulationEvent(..), subscribe
  , setup, manyBody, center, withStrength, withX, withY, static
  )
import PSD3.AST as A
import PSD3.Unified.Attribute as Attr
import PSD3.Unified.Display (showNumD)
import PSD3.Internal.Selection.Types (ElementType(..))

main :: Effect Unit
main = do
  -- Run simulation with D3 engine (or use WASM for same API)
  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "physics"
        [ manyBody "charge" # withStrength (static (-50.0))
        , center "center" # withX (static 200.0) # withY (static 150.0)
        ]
    , nodes:
        [ { id: 0, x: 190.0, y: 140.0, vx: 0.0, vy: 0.0, fx: Nullable.null, fy: Nullable.null }
        , { id: 1, x: 200.0, y: 150.0, vx: 0.0, vy: 0.0, fx: Nullable.null, fy: Nullable.null }
        , { id: 2, x: 210.0, y: 160.0, vx: 0.0, vy: 0.0, fx: Nullable.null, fy: Nullable.null }
        ]
    , links: []
    , container: "#visualization"
    , nodeTemplate: \_ -> A.elem Circle
        [ Attr.attr "cx" _.x showNumD
        , Attr.attr "cy" _.y showNumD
        , Attr.attrStatic "r" "8"
        , Attr.attrStatic "fill" "#4a9eff"
        ]
    , alphaMin: 0.001
    }

  -- Subscribe to simulation events
  _ <- subscribe events \event -> case event of
    Tick { alpha } -> log $ "Alpha: " <> show alpha
    Completed -> log "Simulation converged!"
    _ -> pure unit

  log "Simulation running!"
```

## Features

- **Dual Engine Support** - Same API for D3.js and Rust/WASM physics
- **Framework Agnostic** - Works with Halogen, React, or vanilla JS
- **Declarative Forces** - Compose forces with a fluent builder API
- **Event Subscription** - React to Tick, Started, Stopped, Completed events
- **GUP Semantics** - Enter/update/exit tracking when data changes

## Halogen Integration

```purescript
import PSD3.ForceEngine.Halogen (toHalogenEmitter)

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

- `PSD3.Simulation` - High-level API (recommended)
- `PSD3.Simulation.Emitter` - Framework-agnostic event system
- `PSD3.ForceEngine.Setup` - Declarative force configuration
- `PSD3.ForceEngine.Simulation` - Low-level D3 simulation control

## Part of PSD3

This package is part of the PSD3 visualization ecosystem:
- **psd3-selection** - D3 selection and rendering
- **psd3-simulation** - Force simulation (this package)
- **psd3-simulation-halogen** - Halogen integration
- **psd3-graph** - Graph data structures
- **psd3-layout** - Layout algorithms

## License

MIT
