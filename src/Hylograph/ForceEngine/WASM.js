// WASM Force Engine FFI
// Bridges PureScript to the Rust/WASM force simulation kernel

let wasmModule = null;
let WasmSimulation = null;

// Initialize WASM module (call once at startup)
// EffectFnAff expects (onError, onSuccess) callbacks
export function initWasm_(wasmUrl) {
  return function(onError, onSuccess) {
    (async function() {
      if (wasmModule) {
        console.log('WASM already initialized');
        return;
      }
      // Dynamic import of the WASM module
      const wasm = await import(wasmUrl);
      await wasm.default(); // Initialize WASM
      wasmModule = wasm;
      WasmSimulation = wasm.Simulation;
      console.log('WASM Force Engine initialized');
    })().then(
      // Success handler - EffectFnAff callbacks handle the thunk internally
      () => { onSuccess(); },
      // Error handler - only for actual WASM load failures
      (err) => { onError(err); }
    );

    // Return canceler function (can't really cancel dynamic import)
    return function(cancelError, onCancelError, onCancelSuccess) {
      onCancelSuccess();
    };
  };
}

// Install a module some other code has already instantiated (the namespace,
// or any object with the loader's exports). Like initWasm_, the first module
// installed stays.
export function installKernel_(kernelModule) {
  if (wasmModule) {
    console.log('WASM already initialized');
    return;
  }
  wasmModule = kernelModule;
  WasmSimulation = kernelModule.Simulation;
  console.log('WASM Force Engine initialized');
}

// Check if WASM is initialized
export function isWasmReady_() {
  return wasmModule !== null;
}

// Create a new WASM simulation
export function createSimulation_(nodeCount) {
  return function() {
    if (!WasmSimulation) {
      throw new Error('WASM not initialized. Call initWasm first.');
    }
    return new WasmSimulation(nodeCount);
  };
}

// Free WASM simulation memory
export function freeSimulation_(sim) {
  return function() {
    if (sim && sim.free) {
      sim.free();
    }
  };
}

// Set node count (resizes internal arrays)
export function setNodeCount_(sim) {
  return function(count) {
    return function() {
      sim.set_node_count(count);
    };
  };
}

// Set links from flat array [source0, target0, source1, target1, ...]
export function setLinks_(sim) {
  return function(links) {
    return function() {
      sim.set_links(new Uint32Array(links));
    };
  };
}

// Set a single node's position
export function setPosition_(sim) {
  return function(index) {
    return function(x) {
      return function(y) {
        return function() {
          sim.set_position(index, x, y);
        };
      };
    };
  };
}

// Set all positions from flat array [x0, y0, x1, y1, ...]
export function setAllPositions_(sim) {
  return function(positions) {
    return function() {
      const nodeCount = sim.node_count();
      for (let i = 0; i < nodeCount && i * 2 + 1 < positions.length; i++) {
        sim.set_position(i, positions[i * 2], positions[i * 2 + 1]);
      }
    };
  };
}

// Get all positions as flat array
export function getPositions_(sim) {
  return function() {
    return Array.from(sim.get_positions());
  };
}

// Get positions as array of {x, y} objects
export function getPositionsAsObjects_(sim) {
  return function() {
    const positions = sim.get_positions();
    const result = [];
    const nodeCount = sim.node_count();
    for (let i = 0; i < nodeCount; i++) {
      result.push({
        x: positions[i * 2],
        y: positions[i * 2 + 1]
      });
    }
    return result;
  };
}

// Fix a node at a position (like D3's fx/fy)
export function fixNode_(sim) {
  return function(index) {
    return function(x) {
      return function(y) {
        return function() {
          sim.fix_node(index, x, y);
        };
      };
    };
  };
}

// Unfix a node
export function unfixNode_(sim) {
  return function(index) {
    return function() {
      sim.unfix_node(index);
    };
  };
}

// Configure many-body force
export function configureManyBody_(sim) {
  return function(config) {
    return function() {
      sim.configure_many_body(
        config.strength,
        config.theta,
        config.distanceMin,
        config.distanceMax
      );
    };
  };
}

// Configure link force
export function configureLinks_(sim) {
  return function(config) {
    return function() {
      sim.configure_links(
        config.distance,
        config.strength,
        config.iterations
      );
    };
  };
}

// Configure center force
export function configureCenter_(sim) {
  return function(config) {
    return function() {
      sim.configure_center(config.x, config.y, config.strength);
    };
  };
}

// Enable/disable forces
export function enableForces_(sim) {
  return function(manyBody) {
    return function(links) {
      return function(center) {
        return function() {
          sim.enable_force(manyBody, links, center);
        };
      };
    };
  };
}

// Set alpha (temperature)
export function setAlpha_(sim) {
  return function(alpha) {
    return function() {
      sim.set_alpha(alpha);
    };
  };
}

// Get current alpha
export function getAlpha_(sim) {
  return function() {
    return sim.get_alpha();
  };
}

// Set alpha decay rate
export function setAlphaDecay_(sim) {
  return function(decay) {
    return function() {
      sim.set_alpha_decay(decay);
    };
  };
}

// Set the share of its velocity a node keeps each tick
export function setVelocityRetention_(sim) {
  return function(retention) {
    return function() {
      sim.set_velocity_retention(retention);
    };
  };
}

// Reheat simulation (reset alpha to 1.0)
export function reheat_(sim) {
  return function() {
    sim.reheat();
  };
}

// Run a single simulation tick, returns new alpha
export function tick_(sim) {
  return function() {
    return sim.tick();
  };
}

// Run multiple ticks
export function tickN_(sim) {
  return function(n) {
    return function() {
      return sim.tick_n(n);
    };
  };
}

// Check if simulation is still running (alpha >= alphaMin)
export function isRunning_(sim) {
  return function() {
    return sim.is_running();
  };
}

// Get node count
export function nodeCount_(sim) {
  return function() {
    return sim.node_count();
  };
}

// Get link count
export function linkCount_(sim) {
  return function() {
    return sim.link_count();
  };
}

// Copy positions from PureScript node array to WASM
export function syncPositionsToWasm_(sim) {
  return function(nodes) {
    return function() {
      for (let i = 0; i < nodes.length; i++) {
        const node = nodes[i];
        sim.set_position(i, node.x, node.y);
      }
    };
  };
}

// Copy positions from WASM back to PureScript node array (mutates nodes!)
export function syncPositionsFromWasm_(sim) {
  return function(nodes) {
    return function() {
      const positions = sim.get_positions();
      for (let i = 0; i < nodes.length; i++) {
        nodes[i].x = positions[i * 2];
        nodes[i].y = positions[i * 2 + 1];
        // Also zero velocities in PureScript (WASM manages its own)
        nodes[i].vx = 0;
        nodes[i].vy = 0;
      }
      return nodes;
    };
  };
}

// =============================================================================
// Force list, per-node arrays, masks (uncurried: EffectFn)
// =============================================================================
//
// The kernel never calls back into JS. Anything that varies per node or per
// link is evaluated on the PureScript side and handed over here as an array.

// Remove every force from the kernel's force list
export function clearForces_(sim) {
  sim.clear_forces();
}

// Number of forces in the kernel's force list
export function forceCount_(sim) {
  return sim.force_count();
}

export function addManyBody_(sim, strength, theta, distanceMin, distanceMax) {
  return sim.add_many_body(strength, theta, distanceMin, distanceMax);
}

export function addLinks_(sim, distance, strength, iterations) {
  return sim.add_links(distance, strength, iterations);
}

export function addCenter_(sim, x, y, strength) {
  return sim.add_center(x, y, strength);
}

export function addForceX_(sim, target, strength) {
  return sim.add_force_x(target, strength);
}

export function addForceY_(sim, target, strength) {
  return sim.add_force_y(target, strength);
}

export function addCollide_(sim, radius, strength, iterations) {
  return sim.add_collide(radius, strength, iterations);
}

export function addRadial_(sim, radius, x, y, strength) {
  return sim.add_radial(radius, x, y, strength);
}

// One strength per node (many-body, x, y, radial) or per link (link)
export function setForceStrengths_(sim, force, strengths) {
  return sim.set_force_strengths(force, Float32Array.from(strengths));
}

// One radius per node (collide, radial)
export function setForceRadii_(sim, force, radii) {
  return sim.set_force_radii(force, Float32Array.from(radii));
}

// One target coordinate per node (x, y)
export function setForceTargets_(sim, force, targets) {
  return sim.set_force_targets(force, Float32Array.from(targets));
}

// One rest length per link (link)
export function setForceDistances_(sim, force, distances) {
  return sim.set_force_distances(force, Float32Array.from(distances));
}

// One boolean per node: false = the node has strength 0 under this force
export function setForceMask_(sim, force, mask) {
  return sim.set_force_mask(force, Uint8Array.from(mask, (keep) => (keep ? 1 : 0)));
}

// =============================================================================
// Built-in slots the kernel always had but the bindings did not reach
// =============================================================================

export function configureCollide_(sim, radius, strength, iterations) {
  sim.configure_collide(radius, strength, iterations);
}

export function configureForceX_(sim, target, strength) {
  sim.configure_force_x(target, strength);
}

export function configureForceY_(sim, target, strength) {
  sim.configure_force_y(target, strength);
}

export function enableForcesAll_(sim, manyBody, links, center, forceX, forceY, collide) {
  sim.enable_forces_all(manyBody, links, center, forceX, forceY, collide);
}

// =============================================================================
// Simulation parameters and state
// =============================================================================

export function setAlphaMin_(sim, alphaMin) {
  sim.set_alpha_min(alphaMin);
}

export function setAlphaTarget_(sim, alphaTarget) {
  sim.set_alpha_target(alphaTarget);
}

// Rewind the stream of nudges that separates coincident nodes
export function resetRandom_(sim) {
  sim.reset_random();
}

// Copy position, velocity and pinning (fx/fy, null = free) from PureScript
// nodes to the kernel, in three bulk calls.
export function syncNodesToWasm_(sim, nodes) {
  const n = nodes.length;
  const positions = new Float32Array(n * 2);
  const velocities = new Float32Array(n * 2);
  const fixed = new Float32Array(n * 2);
  for (let i = 0; i < n; i++) {
    const node = nodes[i];
    positions[i * 2] = node.x;
    positions[i * 2 + 1] = node.y;
    velocities[i * 2] = node.vx;
    velocities[i * 2 + 1] = node.vy;
    fixed[i * 2] = node.fx == null ? NaN : node.fx;
    fixed[i * 2 + 1] = node.fy == null ? NaN : node.fy;
  }
  sim.set_positions(positions);
  sim.set_velocities(velocities);
  sim.set_fixed(fixed);
}

// Copy only the pinning (fx/fy, null = free) from PureScript nodes to the
// kernel. Called before every tick: the nodes are where a drag sets and
// clears a pin, and the kernel has to hear of it.
export function syncPinsToWasm_(sim, nodes) {
  const n = nodes.length;
  const fixed = new Float32Array(n * 2);
  for (let i = 0; i < n; i++) {
    const node = nodes[i];
    fixed[i * 2] = node.fx == null ? NaN : node.fx;
    fixed[i * 2 + 1] = node.fy == null ? NaN : node.fy;
  }
  sim.set_fixed(fixed);
}

// Copy position and velocity from the kernel back to PureScript nodes
// (mutates nodes!)
// New records, not the old ones written into: the nodes a caller was handed
// after one tick are PureScript values, and must still say what they said
// when the next tick has been made.
export function syncNodesFromWasm_(sim, nodes) {
  const positions = sim.get_positions();
  const velocities = sim.get_velocities();
  const updated = new Array(nodes.length);
  for (let i = 0; i < nodes.length; i++) {
    updated[i] = {
      ...nodes[i],
      x: positions[i * 2],
      y: positions[i * 2 + 1],
      vx: velocities[i * 2],
      vy: velocities[i * 2 + 1],
    };
  }
  return updated;
}

// =============================================================================
// Forces d3-force does not have (the kernel documents each rule)
// =============================================================================

// "No group / no region" for a node: any negative number here, 0xFFFFFFFF there.
const ids = (values) => Uint32Array.from(values, (v) => (v < 0 ? 0xFFFFFFFF : v));

export function addTowards_(sim, strength) {
  return sim.add_towards(strength);
}

// Targets as [x0, y0, x1, y1, ...]; NaN leaves a node alone.
export function setForceTargetsXY_(sim, force, targets) {
  return sim.set_force_targets_xy(force, Float32Array.from(targets));
}

export function addRectCollide_(sim, halfWidth, halfHeight, strength, iterations) {
  return sim.add_rect_collide(halfWidth, halfHeight, strength, iterations);
}

export function setForceHalfWidths_(sim, force, halfWidths) {
  return sim.set_force_half_widths(force, Float32Array.from(halfWidths));
}

export function setForceHalfHeights_(sim, force, halfHeights) {
  return sim.set_force_half_heights(force, Float32Array.from(halfHeights));
}

export function addContainment_(sim, strength, hard) {
  return sim.add_containment(strength, hard);
}

export function containmentAddRect_(sim, force, x0, y0, x1, y1) {
  return sim.containment_add_rect(force, x0, y0, x1, y1);
}

export function containmentAddCircle_(sim, force, x, y, radius) {
  return sim.containment_add_circle(force, x, y, radius);
}

export function containmentAddPolygon_(sim, force, vertices) {
  return sim.containment_add_polygon(force, Float32Array.from(vertices));
}

// One region index per node; negative = free
export function setForceRegions_(sim, force, regions) {
  return sim.set_force_regions(force, ids(regions));
}

export function addCohesion_(sim, strength) {
  return sim.add_cohesion(strength);
}

// One group id per node; negative = in no group
export function setForceGroups_(sim, force, groups) {
  return sim.set_force_groups(force, ids(groups));
}

// 0 = every pair, 1 = within groups, 2 = between groups
export function setManyBodyScope_(sim, force, scope) {
  return sim.set_many_body_scope(force, scope);
}

// 0 = every pair, 1 = within groups, 2 = between groups
export function setRectCollidePairs_(sim, force, pairs) {
  return sim.set_rect_collide_pairs(force, pairs);
}

export function setRectCollideThrough_(sim, force, through) {
  return sim.set_rect_collide_through(force, through);
}

export function setDirectionalHard_(sim, force, hard) {
  return sim.set_directional_hard(force, hard);
}

export function addDirectional_(sim, gap, axisX, axisY, strength, iterations) {
  return sim.add_directional(gap, axisX, axisY, strength, iterations);
}

export function setForceGaps_(sim, force, gaps) {
  return sim.set_force_gaps(force, Float32Array.from(gaps));
}

// =============================================================================
// Snapshot, restore, convergence
// =============================================================================

// A Float64Array holding everything that changes as the simulation runs
export function snapshot_(sim) {
  return sim.snapshot();
}

export function restore_(sim, snapshot) {
  return sim.restore(snapshot);
}

// The nodes with the pins a snapshot recorded (its last 2n numbers, NaN =
// free), as new records.
export function withPinsFromSnapshot_(snapshot, nodes) {
  const base = snapshot.length - nodes.length * 2;
  return nodes.map((node, i) => {
    const fx = snapshot[base + i * 2], fy = snapshot[base + i * 2 + 1];
    return { ...node, fx: Number.isNaN(fx) ? null : fx, fy: Number.isNaN(fy) ? null : fy };
  });
}

export function maxAcceleration_(sim) {
  return sim.max_acceleration();
}

export function settled_(sim, step, acceleration) {
  return sim.settled(step, acceleration);
}

// Collide: strength scaled by alpha, or not (d3's)
export function setCollideAlphaScaled_(sim, force, scaled) {
  return sim.set_collide_alpha_scaled(force, scaled);
}

// Collide: one boolean per node, false = the node collides with nothing
export function setCollideFilter_(sim, force, mask) {
  return sim.set_collide_filter(force, Uint8Array.from(mask, (keep) => (keep ? 1 : 0)));
}

export function kineticEnergy_(sim) {
  return sim.kinetic_energy();
}

export function maxDisplacement_(sim) {
  return sim.max_displacement();
}
