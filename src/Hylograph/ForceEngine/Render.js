// FFI for fast tick updates
//
// Sets attributes straight from the datum bound to each element as
// `__data__` (the convention the selection library keeps). Plain DOM calls:
// no data join and no D3.

const within = (selector, what) => {
  const group = document.querySelector(selector);
  return group ? group.querySelectorAll(what) : [];
};

// Update circle positions (cx, cy) from bound data's x, y fields
export const updateCirclePositions_ = (selector) => () => {
  for (const el of within(selector, "circle")) {
    const d = el.__data__;
    el.setAttribute("cx", d.x);
    el.setAttribute("cy", d.y);
  }
};

// Update line positions from swizzled link data
// Expects data with source.x, source.y, target.x, target.y
export const updateLinkPositions_ = (selector) => () => {
  for (const el of within(selector, "line")) {
    const d = el.__data__;
    el.setAttribute("x1", d.source.x);
    el.setAttribute("y1", d.source.y);
    el.setAttribute("x2", d.target.x);
    el.setAttribute("y2", d.target.y);
  }
};

// Update group positions (transform) from bound data's x, y fields
// For bubble packs and other grouped visualizations
export const updateGroupPositions_ = (selector) => () => {
  for (const el of within(selector, "g.module-pack")) {
    const d = el.__data__;
    el.setAttribute("transform", `translate(${d.x},${d.y})`);
  }
};

// Update tree link paths using current node positions
// Links have source/target as node IDs; we look up positions from the node circles
export const updateTreeLinkPaths_ = (selector) => () => {
  // Build a map from node ID to current position from circle elements
  const nodePositions = {};
  for (const el of within("#explorer-nodes", "circle")) {
    const d = el.__data__;
    if (d && d.id !== undefined) {
      nodePositions[d.id] = { x: d.x, y: d.y };
    }
  }

  for (const el of within(selector, "path.tree-link")) {
    const d = el.__data__;
    const source = nodePositions[d.source];
    const target = nodePositions[d.target];
    if (!source || !target) {
      el.setAttribute("d", "");
      continue;
    }
    // Vertical bezier link path (same logic as PureScript verticalLinkPath)
    const midY = (source.y + target.y) / 2.0;
    el.setAttribute("d", `M${source.x},${source.y}C${source.x},${midY} ${target.x},${midY} ${target.x},${target.y}`);
  }
};

// Generic position update with custom callback, which is given every
// element under the group.
export const updatePositions_ = (selector) => (callback) => () => {
  callback(Array.from(within(selector, "*")))();
};
