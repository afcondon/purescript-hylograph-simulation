# purescript-psd3-simulation

Force-directed graph simulation for PureScript D3.

## Overview

A pure PureScript implementation of D3-style force simulation for interactive graph layouts. Provides force-directed positioning with configurable forces and tick-based animation.

## Installation

```bash
spago install psd3-simulation
```

## Features

### Force Simulation

- **D3-compatible API** - Familiar simulation/force pattern
- **Tick-based updates** - Integrates with Halogen subscriptions
- **Configurable forces** - Link, charge, center, collision forces
- **Alpha decay** - Smooth convergence to stable state

### Integration

Works seamlessly with psd3-selection for rendering force-directed graphs and networks.

## Example

```purescript
import PSD3.Simulation (createSimulation, forceLink, forceCharge, forceCenter)

simulation = createSimulation nodes
  # forceLink links
  # forceCharge (-30.0)
  # forceCenter (width / 2.0) (height / 2.0)
```

## Modules

- `PSD3.Simulation` - Main simulation API
- `PSD3.Simulation.Force` - Force implementations
- `PSD3.Simulation.Types` - Core types

## Part of PSD3

- **psd3-tree** - Tree data structures (dependency)
- **psd3-selection** - D3 selection library (dependency)
- **psd3-simulation** - Force simulation (this package)
- **psd3-layout** - Layout algorithms

## License

MIT
