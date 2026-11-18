-- | Force Simulation Rendering
-- |
-- | FFI-optimized helpers for rendering force-directed graphs.
-- |
-- | IMPORTANT PATTERN: For simulations, bind data ONCE then update positions via FFI.
-- | Do NOT use Tree API or data joins in tick callbacks - they are O(n) per tick.
-- |
-- | Correct usage:
-- | ```purescript
-- | -- Define group IDs (type-safe, no typos)
-- | nodesGroup = GroupId "#my-nodes"
-- | linksGroup = GroupId "#my-links"
-- |
-- | -- At initialization (once):
-- | nodeSel <- appendData Circle nodes [cx _.x, cy _.y, ...]
-- | linkSel <- appendData Line swizzledLinks [x1 (\l -> l.source.x), ...]
-- |
-- | -- In tick callback (every frame):
-- | updateCirclePositions nodesGroup
-- | updateLinkPositions linksGroup
-- | ```
-- |
-- | The FFI updates set attributes straight from each element's bound datum:
-- | O(n) DOM updates with no data join, and no D3.
module Hylograph.ForceEngine.Render
  ( -- * Type-safe Group IDs
    GroupId(..)
    -- * Position Updates (FFI-optimized)
  , updateCirclePositions
  , updateLinkPositions
  , updateGroupPositions
  , updateTreeLinkPaths
    -- * Custom Position Updates
  , updatePositions
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Web.DOM.Element (Element)

-- =============================================================================
-- Type-safe Group IDs
-- =============================================================================

-- | A type-safe wrapper for DOM selector strings.
-- |
-- | Use this to identify groups of elements for position updates.
-- | The newtype provides compile-time checking that you're using
-- | group IDs consistently, without any runtime overhead.
-- |
-- | Example:
-- | ```purescript
-- | -- Define once, use everywhere
-- | packageNodes :: GroupId
-- | packageNodes = GroupId "#package-nodes"
-- |
-- | moduleNodes :: GroupId
-- | moduleNodes = GroupId "#module-nodes"
-- | ```
newtype GroupId = GroupId String

derive instance Newtype GroupId _
derive newtype instance Eq GroupId
derive newtype instance Ord GroupId
derive newtype instance Show GroupId

-- =============================================================================
-- FFI Imports (internal - take raw strings)
-- =============================================================================

foreign import updateCirclePositions_ :: String -> Effect Unit
foreign import updateLinkPositions_ :: String -> Effect Unit
foreign import updateGroupPositions_ :: String -> Effect Unit
foreign import updateTreeLinkPaths_ :: String -> Effect Unit
foreign import updatePositions_ :: String -> (Array Element -> Effect Unit) -> Effect Unit

-- =============================================================================
-- Public API (type-safe GroupId)
-- =============================================================================

-- | Update circle positions (cx, cy) from bound data's x, y fields.
-- |
-- | Assumes data bound to circles has `x :: Number` and `y :: Number` fields.
-- | Uses D3's `selection.attr()` which reads from `__data__` on each element.
-- |
-- | Example:
-- | ```purescript
-- | nodesGroup :: GroupId
-- | nodesGroup = GroupId "#nodes"
-- |
-- | updateCirclePositions nodesGroup
-- | ```
updateCirclePositions :: GroupId -> Effect Unit
updateCirclePositions gid = updateCirclePositions_ (unwrap gid)

-- | Update line positions (x1, y1, x2, y2) from bound data's source/target nodes.
-- |
-- | Assumes data bound to lines has:
-- | - `source :: { x :: Number, y :: Number, ... }`
-- | - `target :: { x :: Number, y :: Number, ... }`
-- |
-- | This is the "swizzled link" format where source/target are node objects,
-- | not integer indices. Use `Sim.getSwizzledLinks` after adding a Link force.
-- |
-- | Example:
-- | ```purescript
-- | linksGroup :: GroupId
-- | linksGroup = GroupId "#links"
-- |
-- | updateLinkPositions linksGroup
-- | ```
updateLinkPositions :: GroupId -> Effect Unit
updateLinkPositions gid = updateLinkPositions_ (unwrap gid)

-- | Update group positions (transform) from bound data's x, y fields.
-- |
-- | Selects `g.module-pack` elements and updates their transform attribute.
-- | Assumes data bound to groups has `x :: Number` and `y :: Number` fields.
-- |
-- | Example:
-- | ```purescript
-- | nodesGroup :: GroupId
-- | nodesGroup = GroupId "#nodes"
-- |
-- | updateGroupPositions nodesGroup
-- | ```
updateGroupPositions :: GroupId -> Effect Unit
updateGroupPositions gid = updateGroupPositions_ (unwrap gid)

-- | Update tree link paths (d attribute) from bound data's source/target nodes.
-- |
-- | Expects data bound to path elements has:
-- | - `source :: Int` (node ID)
-- | - `target :: Int` (node ID)
-- |
-- | This function looks up node positions from the DOM (circles) and computes
-- | vertical bezier paths. Used for animating tree links during transitions.
-- |
-- | Example:
-- | ```purescript
-- | linksGroup :: GroupId
-- | linksGroup = GroupId "#explorer-links"
-- |
-- | updateTreeLinkPaths linksGroup
-- | ```
updateTreeLinkPaths :: GroupId -> Effect Unit
updateTreeLinkPaths gid = updateTreeLinkPaths_ (unwrap gid)

-- | Generic position update with a custom setter, which is given every
-- | element under the group; each element's bound datum is its `__data__`.
-- | (Until 2026-11-18 the setter was handed a D3 selection.)
updatePositions :: GroupId -> (Array Element -> Effect Unit) -> Effect Unit
updatePositions gid = updatePositions_ (unwrap gid)
