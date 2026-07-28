-- | Force configuration for force-directed layouts.
-- |
-- | `ForceSpec` and its per-force configuration records — what forces to
-- | apply and how strongly. Pure data; interpreted by
-- | `Hylograph.ForceEngine.Simulation`.
-- |
-- | This module does NOT define the node or link types. Those live in
-- | `Hylograph.ForceEngine.Simulation` as `SimulationNode` and `Link`,
-- | which is what `setNodes` and `setLinks` consume.
module Hylograph.ForceEngine.Types
  ( -- * Simulation Parameters
    defaultSimParams
    -- * Force Configurations
  , ManyBodyConfig
  , defaultManyBody
  , CollideConfig
  , defaultCollide
  , LinkConfig
  , defaultLink
  , CenterConfig
  , defaultCenter
  , ForceXConfig
  , ForceYConfig
  , RadialConfig
    -- * Filtered/Dynamic Configurations
  , ManyBodyFilteredConfig
  , RadialFilteredConfig
  , CollideDynamicConfig
  , ForceXDynamicConfig
  , ForceYDynamicConfig
  , LinkDynamicConfig
    -- * Force Specification
  , ForceSpec(..)
  , forceName
  ) where

import Prelude


-- =============================================================================
-- Simulation Parameters
-- =============================================================================
--
-- Note: the node, link and simulation-state types that used to live here
-- were removed in 0.6.0. They had no importers anywhere in the ecosystem
-- and duplicated the types in hylograph-d3-kernel, while carrying the more
-- inviting names — `SimNode` next to the live `SimulationNode`, in a
-- module called `Types`. That combination actively misled: the shapes are
-- subtly incompatible (`index` where the live type has `id`, and no
-- `fx`/`fy`), so code written against them typechecks in isolation and
-- cannot be handed to `Sim.setNodes`.
--
-- The live types are in `Hylograph.ForceEngine.Simulation`:
-- `SimulationNode` and `Link`.

-- | Default simulation parameters
defaultSimParams ::
  { alpha :: Number
  , alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  , velocityDecay :: Number
  }
defaultSimParams =
  { alpha: 1.0
  , alphaMin: 0.001
  , alphaDecay: 0.01  -- Slower cooling than D3 default (0.0228) for better settling
  , alphaTarget: 0.0
  , velocityDecay: 0.4  -- D3's default
  }

-- =============================================================================
-- Force Configuration (Pure Data)
-- =============================================================================

-- | Configuration for a many-body (charge) force
type ManyBodyConfig =
  { strength :: Number       -- Positive = attract, negative = repel
  , theta :: Number          -- Barnes-Hut approximation parameter (0.9 default)
  , distanceMin :: Number    -- Minimum distance (avoids infinite forces)
  , distanceMax :: Number    -- Maximum distance (limits range)
  }

defaultManyBody :: ManyBodyConfig
defaultManyBody =
  { strength: -30.0
  , theta: 0.9
  , distanceMin: 1.0
  , distanceMax: infinity
  }
  where
  infinity = 1.0e10  -- Practical infinity

-- | Configuration for a collision force
type CollideConfig =
  { radius :: Number         -- Collision radius (or function of node)
  , strength :: Number       -- How strongly to resolve collisions (0-1)
  , iterations :: Int        -- Number of iterations per tick
  }

defaultCollide :: CollideConfig
defaultCollide =
  { radius: 1.0
  , strength: 1.0
  , iterations: 1
  }

-- | Configuration for a link force
type LinkConfig =
  { distance :: Number       -- Desired link length
  , strength :: Number       -- Spring strength (0-1)
  , iterations :: Int        -- Number of iterations per tick
  }

defaultLink :: LinkConfig
defaultLink =
  { distance: 30.0
  , strength: 1.0
  , iterations: 1
  }

-- | Configuration for centering force
type CenterConfig =
  { x :: Number              -- Center x
  , y :: Number              -- Center y
  , strength :: Number       -- How strongly to center (0-1)
  }

defaultCenter :: CenterConfig
defaultCenter =
  { x: 0.0
  , y: 0.0
  , strength: 1.0
  }

-- | Configuration for X positioning force
type ForceXConfig =
  { x :: Number              -- Target x position
  , strength :: Number       -- How strongly to pull (0-1)
  }

-- | Configuration for Y positioning force
type ForceYConfig =
  { y :: Number              -- Target y position
  , strength :: Number       -- How strongly to pull (0-1)
  }

-- | Configuration for radial force
type RadialConfig =
  { radius :: Number         -- Target radius from center
  , x :: Number              -- Center x
  , y :: Number              -- Center y
  , strength :: Number       -- How strongly to pull (0-1)
  }

-- =============================================================================
-- Filtered/Dynamic Force Configurations
-- =============================================================================
-- These allow forces to be applied conditionally or with dynamic parameters.
-- The accessor functions are passed to JS which calls them per-node.

-- | Configuration for filtered many-body force
-- | Applies charge only to nodes matching the predicate
type ManyBodyFilteredConfig node =
  { strength :: Number
  , theta :: Number
  , distanceMin :: Number
  , distanceMax :: Number
  , filter :: node -> Boolean  -- Only apply to matching nodes
  }

-- | Configuration for filtered radial force
type RadialFilteredConfig node =
  { radius :: Number
  , x :: Number
  , y :: Number
  , strength :: Number
  , filter :: node -> Boolean  -- Only apply to matching nodes
  }

-- | Configuration for collision force with dynamic radius
-- | Radius is computed per-node via accessor function
type CollideDynamicConfig node =
  { radiusAccessor :: node -> Number  -- Get radius from node (e.g., d.r + padding)
  , strength :: Number
  , iterations :: Int
  }

-- | Configuration for X positioning force with dynamic target
-- | Target X is computed per-node via accessor function
type ForceXDynamicConfig node =
  { xAccessor :: node -> Number  -- Get target X from node
  , strength :: Number
  }

-- | Configuration for Y positioning force with dynamic target
type ForceYDynamicConfig node =
  { yAccessor :: node -> Number  -- Get target Y from node
  , strength :: Number
  }

-- | Configuration for link force with dynamic strength per-link
-- | Strength is computed per-link via accessor function
type LinkDynamicConfig link =
  { distance :: Number              -- Desired link length
  , strengthAccessor :: link -> Number  -- Get strength from link (e.g., link.weight)
  , iterations :: Int
  }

-- =============================================================================
-- Force Sum Type
-- =============================================================================

-- | A force configuration - pure data describing a force
data ForceSpec
  = ManyBody String ManyBodyConfig      -- name, config
  | Collide String CollideConfig
  | Link String LinkConfig
  | Center String CenterConfig
  | PositionX String ForceXConfig
  | PositionY String ForceYConfig
  | Radial String RadialConfig

-- | Get the name of a force
forceName :: ForceSpec -> String
forceName = case _ of
  ManyBody name _ -> name
  Collide name _ -> name
  Link name _ -> name
  Center name _ -> name
  PositionX name _ -> name
  PositionY name _ -> name
  Radial name _ -> name

