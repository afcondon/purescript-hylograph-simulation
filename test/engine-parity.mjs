// Does one declarative Setup mean the same thing to the library's two
// engines: the default one (PureScript, hylograph-forces) and the
// Rust/WASM kernel?
//
//   spago build && node test/engine-parity.mjs
//
// The forces themselves are checked elsewhere, exactly: hylograph-forces
// against d3-force and against the kernel at f64 (its test/differential.mjs).
// This checks the layer above, the PureScript that turns a `Setup` into
// forces (`Hylograph.ForceEngine.Setup.recipe`) and into kernel calls
// (`Hylograph.ForceEngine.Setup.WASM`), by driving the compiled PureScript of
// both engines from Node with the same nodes, links and Setup:
//
//   1. the seven d3 forces: two many-body forces, filters, per-node values, a
//      force name used twice, pinned nodes, links that name nodes by id;
//   2. GUP: after 2 ticks nodes are removed, added and reordered through
//      applySetupWithData on both engines, and the next tick must still
//      agree (positions, velocities and pins have to survive the same way);
//   3. per-node strengths and radii on many-body, x, y and radial;
//   4. per-link distance and strength, degree strength, filtered x / y;
//   5. the forces d3-force does not have, each alone and all together;
//   6. what only the default engine promises: the same layout on every run,
//      to the bit;
//   7. what both have to hear: a link to a missing node (an error, in d3's
//      words, on both), the setup's params whatever the simulation was
//      created with, a pin set on a node between ticks, pins in a snapshot.
//
// The shipped WASM kernel is f32 and the default engine is f64, so the two
// agree to float width (1e-6 of the layout's extent) one tick from identical
// state, which is what is asserted (*); later ticks are reported, since a
// force layout amplifies rounding.
import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const output = join(here, '..', 'output');
const ps = (module) => import(pathToFileURL(join(output, module, 'index.js')));
const Setup = await ps('Hylograph.ForceEngine.Setup');
const PureSim = await ps('Hylograph.ForceEngine.Simulation');
const WasmSetup = await ps('Hylograph.ForceEngine.Setup.WASM');
const Maybe = await ps('Data.Maybe');
const Either = await ps('Data.Either');

// The library loads the kernel with fetch(); give Node a fetch that reads files.
const realFetch = globalThis.fetch;
globalThis.fetch = async (url, ...rest) => {
  const href = String(url instanceof Request ? url.url : url);
  if (!href.startsWith('file:')) return realFetch(url, ...rest);
  return new Response(readFileSync(fileURLToPath(href)), { headers: { 'content-type': 'application/wasm' } });
};
const quietly = (run) => { const log = console.log; console.log = () => {}; try { return run(); } finally { console.log = log; } };
const kernelUrl = pathToFileURL(join(homedir(), 'work/afc-work/purescript-hylograph-showcases/wasm-force-demo/pkg/force_kernel.js')).href;
const { initWasm_ } = await import(pathToFileURL(join(output, 'Hylograph.ForceEngine.WASM/foreign.js')));
await new Promise((resolve, reject) => quietly(() => initWasm_(kernelUrl)(reject, resolve)));

// ---------------------------------------------------------------------------
// Data. Every number is first rounded to f32, so both engines start from
// identical inputs and what is measured is the arithmetic.
// ---------------------------------------------------------------------------
const f = Math.fround;
const N = 300;
const idOf = (i) => 7 + 3 * i; // ids are deliberately not array indices
const spiral = (i) => { const r = 10 * Math.sqrt(0.5 + i), a = i * Math.PI * (3 - Math.sqrt(5)); return [f(r * Math.cos(a)), f(r * Math.sin(a))]; };
const makeNode = (i) => {
  const [x, y] = spiral(i);
  return {
    id: idOf(i), x, y, vx: 0, vy: 0, fx: null, fy: null,
    r: f(3 + (i % 4) * 1.5), gridX: f(((i % 10) - 4.5) * 50), gridY: f((Math.floor(i / 10) % 10 - 4.5) * 50),
    keep: i % 3 !== 0, charge: f(-10 - (i % 7) * 6), pull: f(0.02 + (i % 4) * 0.02), ring: f(80 + (i % 3) * 60),
  };
};
const makeNodes = () => {
  const nodes = Array.from({ length: N }, (_, i) => makeNode(i));
  nodes[4].fx = 20; nodes[4].fy = -20; // pinned on both axes
  nodes[9].fx = -35;                   // on x only
  nodes[15].fy = 45;                   // on y only
  return nodes;
};
const makeLinks = (present) => {
  const links = [];
  for (let i = 0; i < N; i++) {
    links.push([i, (i + 1) % N]);
    if (i % 7 === 0 && (i * 13 + 5) % N !== i) links.push([i, (i * 13 + 5) % N]);
  }
  return links.filter(([a, b]) => present(a) && present(b)).map(([a, b]) => ({ source: idOf(a), target: idOf(b) }));
};
const all = () => true;

const { manyBody, collide, link, center, positionX, positionY, radial } = Setup;
const fixed = Setup.static, perNode = Setup.dynamic;
const just = (value) => Maybe.Just.create(value), nothing = Maybe.Nothing.value;
const build = (force, ...modifiers) => modifiers.reduce((config, modify) => modify(config), force);
const params = { alpha: 1, alphaMin: 0.001, alphaDecay: 0.02, alphaTarget: 0, velocityRetention: 0.6 };

// ---------------------------------------------------------------------------
// The two engines
// ---------------------------------------------------------------------------
const pureEngine = (setup, nodes, links) => {
  const sim = PureSim.create({ alphaMin: params.alphaMin, alphaDecay: params.alphaDecay, alphaTarget: params.alphaTarget, velocityRetention: params.velocityRetention })();
  PureSim.setNodes(nodes)(sim)();
  PureSim.setLinks(links)(sim)();
  Setup.applySetup(setup)(sim)();
  return {
    tick: () => PureSim.tick(sim)(),
    update: (next, nextNodes, nextLinks) => Setup.applySetupWithData(next)(nextNodes)(nextLinks)(sim)(),
    positions: () => new Map(sim.nodes.value.map((d) => [d.id, [d.x, d.y]])),
    nodes: () => sim.nodes.value,
  };
};
const wasmEngine = (setup, nodes, links) => {
  const sim = WasmSetup.create(nodes)(links)();
  WasmSetup.applySetup(setup)(sim)();
  return {
    tick: () => WasmSetup.tick(sim)(),
    update: (next, nextNodes, nextLinks) => WasmSetup.applySetupWithData(next)(nextNodes)(nextLinks)(sim)(),
    positions: () => new Map(WasmSetup.getNodes(sim)().map((d) => [d.id, [d.x, d.y]])),
    nodes: () => WasmSetup.getNodes(sim)(),
  };
};

const compare = (a, b) => {
  if (a.size !== b.size) return { difference: Infinity, relative: Infinity };
  let worst = 0, x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
  for (const [id, [x, y]] of a) {
    const other = b.get(id);
    if (!other) return { difference: Infinity, relative: Infinity };
    worst = Math.max(worst, Math.abs(x - other[0]), Math.abs(y - other[1]));
    x0 = Math.min(x0, x); x1 = Math.max(x1, x); y0 = Math.min(y0, y); y1 = Math.max(y1, y);
  }
  return { difference: worst, relative: worst / Math.max(x1 - x0, y1 - y0) };
};

const FLOAT_WIDTH = 1e-6; // of the extent, one tick from identical state
const failures = [];
const report = (what, tick, { difference, relative }, asserted) => {
  console.log(`   ${what.padEnd(40)} tick ${String(tick).padStart(3)}: max difference ${difference.toExponential(1)} (${relative.toExponential(1)} of the extent)${asserted ? ' *' : ''}`);
  if (asserted && !(relative <= FLOAT_WIDTH)) failures.push(`${what}, tick ${tick}: ${relative.toExponential(2)} of the extent (bound ${FLOAT_WIDTH})`);
};
const check = (ok, what) => { console.log(`   ${ok ? 'ok    ' : 'FAILED'} ${what}`); if (!ok) failures.push(what); };

// One Setup through both engines for 20 ticks; tick 1 asserted.
const both = (what, setup, links = makeLinks(all), assertedAt = 1) => {
  const viaPure = pureEngine(setup, makeNodes(), links);
  const viaWasm = wasmEngine(setup, makeNodes(), links);
  for (let tick = 1; tick <= 20; tick++) {
    viaPure.tick(); viaWasm.tick();
    if (tick === 1 || tick === 10 || tick === 20) report(what, tick, compare(viaPure.positions(), viaWasm.positions()), tick === assertedAt);
  }
  return { viaPure, viaWasm };
};

// ---------------------------------------------------------------------------
// 1 and 2. The same Setup through both engines, then a GUP update
// ---------------------------------------------------------------------------
// Names decide the order: both engines apply forces sorted by name.
const paritySetup = (gentle) => Setup.setupWithParams('parity')([
  build(collide('bump'), Setup.withRadius(fixed(40))), // replaced by the later 'bump'
  build(manyBody('charge'), Setup.withStrength(fixed(-40)), Setup.withFilter((d) => d.keep)),
  build(manyBody('charge-near'), Setup.withStrength(fixed(-15)), Setup.withDistanceMax(120), Setup.withTheta(0.7)),
  build(link('links'), Setup.withDistance(fixed(35)), Setup.withStrength(fixed(gentle)), Setup.withIterations(2)),
  build(collide('bump'), Setup.withRadius(perNode((d) => d.r + 2)), Setup.withStrength(fixed(0.8))),
  build(positionX('gridX'), Setup.withX(perNode((d) => d.gridX)), Setup.withStrength(fixed(0.05))),
  build(positionY('gridY'), Setup.withY(perNode((d) => d.gridY)), Setup.withStrength(fixed(0.05))),
  build(radial('ring'), Setup.withRadius(fixed(200)), Setup.withStrength(fixed(0.03)), Setup.withFilter((d) => !d.keep)),
  build(center('middle'), Setup.withX(fixed(10)), Setup.withY(fixed(-10)), Setup.withStrength(fixed(0.7))),
])(params);

console.log('1. One Setup, two engines: the seven d3 forces (* = asserted to float width)');
both('default engine vs WASM engine', paritySetup(0.02));

{
  // GUP: drop every fifth node, add ten new ones, reverse the order.
  const setup = paritySetup(0.02);
  const viaPure = pureEngine(setup, makeNodes(), makeLinks(all));
  const viaWasm = wasmEngine(setup, makeNodes(), makeLinks(all));
  const kept = (i) => i % 5 !== 2;
  const desired = () => {
    const nodes = makeNodes().filter((_, i) => kept(i));
    for (let k = 0; k < 10; k++) nodes.push({ ...makeNode(N + k), id: 100000 + k, x: f(k * 12 - 60), y: f(260) });
    return nodes.reverse();
  };
  console.log('\n2. applySetupWithData on both after 2 ticks: nodes removed, added and reordered');
  for (let tick = 1; tick <= 2; tick++) { viaPure.tick(); viaWasm.tick(); }
  const fromPure = viaPure.update(setup, desired(), makeLinks(kept));
  const fromWasm = viaWasm.update(setup, desired(), makeLinks(kept));
  const counts = (r) => `${r.nodes.entered.length} entered, ${r.nodes.updated.length} updated, ${r.nodes.exited.length} exited`;
  console.log(`   default engine: ${counts(fromPure)}\n   WASM engine:    ${counts(fromWasm)}`);
  if (counts(fromPure) !== counts(fromWasm)) failures.push('GUP: the two engines categorised the nodes differently');
  for (let tick = 3; tick <= 20; tick++) {
    viaPure.tick(); viaWasm.tick();
    // A velocity or a pin lost in the update would show at tick 3 at full size.
    if (tick === 3 || tick === 10 || tick === 20) report('default engine vs WASM engine', tick, compare(viaPure.positions(), viaWasm.positions()), tick === 3);
  }
}

// ---------------------------------------------------------------------------
// 3. Per-node strengths and radii
// ---------------------------------------------------------------------------
console.log('\n3. Per-node strength and radius on many-body, x, y and radial');
both('default engine vs WASM engine', Setup.setupWithParams('dynamic')([
  build(manyBody('a-charge'), Setup.withStrength(perNode((d) => d.charge))),
  build(positionX('b-x'), Setup.withX(fixed(30)), Setup.withStrength(perNode((d) => d.pull))),
  build(positionY('c-y'), Setup.withY(perNode((d) => d.gridY)), Setup.withStrength(perNode((d) => d.pull))),
  build(radial('d-ring'), Setup.withRadius(perNode((d) => d.ring)), Setup.withStrength(perNode((d) => d.pull)), Setup.withFilter((d) => d.keep)),
])(params));

// ---------------------------------------------------------------------------
// 4. Per-link values, d3's default link strength, a filtered positioning force
// ---------------------------------------------------------------------------
console.log('\n4. Per-link distance and strength, degree strength, filtered x / y');
both('default engine vs WASM engine', Setup.setupWithParams('links')([
  build(link('a-links'), Setup.withLinkDistance(perNode((ends) => 20 + ends.source.r * 4 + (ends.index % 3) * 5)), Setup.withDegreeStrength),
  build(link('b-links'), Setup.withDistance(fixed(60)), Setup.withLinkStrength(perNode((ends) => (ends.target.keep ? 0.05 : 0.02)))),
  build(positionX('c-x'), Setup.withX(perNode((d) => d.gridX)), Setup.withStrength(fixed(0.1)), Setup.withFilter((d) => d.keep)),
  build(positionY('d-y'), Setup.withY(fixed(15)), Setup.withStrength(fixed(0.1)), Setup.withFilter((d) => !d.keep)),
  build(manyBody('e-charge'), Setup.withStrength(fixed(-20))),
])(params));

// ---------------------------------------------------------------------------
// 5. The forces d3-force does not have
// ---------------------------------------------------------------------------
console.log('\n5. Forces d3-force does not have, each alone and then all together');
const groupOf = (d) => (d.id % 11 === 0 ? nothing : just((d.id % 4) * 100)); // groups 0, 100, 200, 300, and some nodes in none
const home = (d) => (d.id % 13 === 0 ? nothing : just({ x: d.gridX, y: d.gridY }));
const circle = Setup.CircleRegion.create({ x: 0, y: 0, radius: 120 });
const box = Setup.RectRegion.create({ x0: -150, y0: -150, x1: -20, y1: 150 });
const hexagon = Setup.PolygonRegion.create([[90, 0], [45, 78], [-45, 78], [-90, 0], [-45, -78], [45, -78]].map(([x, y]) => ({ x, y })));
const newForces = {
  towards: build(Setup.towards('home')(home), Setup.withStrength(perNode((d) => d.pull * 4)), Setup.withFilter((d) => d.keep)),
  rectCollide: build(Setup.rectCollide('labels'), Setup.withHalfWidth(perNode((d) => 6 + d.r)), Setup.withHalfHeight(fixed(4)), Setup.withIterations(2)),
  contain: build(Setup.contain('pen')(circle), Setup.withRegions([circle, box, hexagon]), Setup.withRegion((d) => (d.id % 7 === 0 ? nothing : just(d.id % 3))), Setup.withRadius(perNode((d) => d.r)), Setup.withStrength(fixed(0.6))),
  'contain, soft only': build(Setup.contain('fence')(hexagon), Setup.withHardLimit(false), Setup.withStrength(fixed(0.3))),
  cohesion: build(Setup.cohesion('together')(groupOf), Setup.withStrength(fixed(0.05))),
  directional: build(Setup.directional('below'), Setup.withGap(perNode((ends) => 8 + (ends.index % 4) * 3)), Setup.withLinkStrength(perNode((ends) => (ends.source.keep ? 0.5 : 0.2))), Setup.withAxis({ x: 1, y: 2 })),
  'many-body between groups': build(manyBody('apart'), Setup.withStrength(fixed(-30)), Setup.betweenGroups(groupOf)),
  'many-body within groups': build(manyBody('spread'), Setup.withStrength(perNode((d) => d.charge)), Setup.withinGroups(groupOf)),
  'collide, filtered and scaled by alpha': build(collide('yield'), Setup.withRadius(perNode((d) => d.r + 3)), Setup.withFilter((d) => d.keep), Setup.withAlphaScaling(true)),
};
for (const [what, force] of Object.entries(newForces)) both(what, Setup.setupWithParams(what)([force])(params));
both('all nine together', Setup.setupWithParams('new')(Object.values(newForces))(params));

{
  // Hard containment holds on both: after 60 ticks every unpinned node of the
  // contained setup is inside its own region, radius included.
  const { viaPure, viaWasm } = (() => {
    const setup = Setup.setupWithParams('held')([newForces.contain, build(manyBody('charge'))])(params);
    const viaPure = pureEngine(setup, makeNodes(), []), viaWasm = wasmEngine(setup, makeNodes(), []);
    for (let tick = 0; tick < 60; tick++) { viaPure.tick(); viaWasm.tick(); }
    return { viaPure, viaWasm };
  })();
  const slack = 1e-2;
  const inHexagon = (d) => [[90, 0], [45, 78], [-45, 78], [-90, 0], [-45, -78], [45, -78]].every(([x0, y0], i, vs) => {
    const [x1, y1] = vs[(i + 1) % 6], len = Math.hypot(x1 - x0, y1 - y0);
    return ((d.x - x0) * -(y1 - y0) + (d.y - y0) * (x1 - x0)) / len >= d.r - slack;
  });
  const inside = (d) => d.fx != null || d.fy != null || d.id % 7 === 0 || [
    () => Math.hypot(d.x, d.y) <= 120 - d.r + slack,
    () => d.x >= -150 + d.r - slack && d.x <= -20 - d.r + slack && d.y >= -150 + d.r - slack && d.y <= 150 - d.r + slack,
    () => inHexagon(d),
  ][d.id % 3]();
  check(viaPure.nodes().every(inside), 'default engine: hard containment holds every unpinned node inside its own region');
  check(viaWasm.nodes().every(inside), 'WASM engine: the same');
}

// ---------------------------------------------------------------------------
// 6. What the default engine promises on its own
// ---------------------------------------------------------------------------
console.log('\n6. The default engine alone');
{
  const run = () => {
    const engine = pureEngine(Setup.setupWithParams('again')([...Object.values(newForces), build(collide('bump'), Setup.withRadius(fixed(6)))])(params), makeNodes().map((d) => ({ ...d, x: 0, y: 0 })), makeLinks(all));
    for (let tick = 0; tick < 100; tick++) engine.tick();
    return engine.nodes().map((d) => [d.x, d.y, d.vx, d.vy]);
  };
  const first = run(), second = run();
  check(first.every((values, i) => values.every((value, k) => Object.is(value, second[i][k]))) && first.every((values) => values.every(Number.isFinite)),
    'the same layout twice, to the bit: 300 nodes starting on one spot, 100 ticks, every force');
}
// ---------------------------------------------------------------------------
// 7. Things a Setup or a node says that both engines have to hear
// ---------------------------------------------------------------------------
console.log('\n7. What both engines have to hear');
{
  // A link to a node that is not there: an error on both, in the same words,
  // when the setup has a force along the links; no error when it has none.
  const dangling = [...makeLinks(all), { source: idOf(3), target: 999999 }];
  const withLinks = Setup.setupWithParams('dangling')([build(manyBody('charge')), build(link('links'))])(params);
  const without = Setup.setupWithParams('no link force')([build(manyBody('charge'))])(params);
  const pure = () => { const sim = PureSim.create({ alphaMin: 0.001, alphaDecay: 0.02, alphaTarget: 0, velocityRetention: 0.6 })(); PureSim.setNodes(makeNodes())(sim)(); PureSim.setLinks(dangling)(sim)(); return { tryApply: (setup) => Setup.tryApplySetup(setup)(sim)(), apply: (setup) => Setup.applySetup(setup)(sim)() }; };
  const wasm = () => { const sim = WasmSetup.create(makeNodes())(dangling)(); return { tryApply: (setup) => WasmSetup.tryApplySetup(setup)(sim)(), apply: (setup) => WasmSetup.applySetup(setup)(sim)() }; };
  const outcome = (engine) => {
    const asValue = Either.either((problem) => problem)(() => 'applied')(engine().tryApply(withLinks));
    let thrown = 'no error';
    try { engine().apply(withLinks); } catch (error) { thrown = error.message; }
    const harmless = Either.either((problem) => problem)(() => 'applied')(engine().tryApply(without));
    return [asValue, thrown, harmless];
  };
  const fromPure = outcome(pure), fromWasm = outcome(wasm);
  check(fromPure[0] === 'node not found: 999999' && fromPure[1].includes('node not found: 999999') && fromPure[2] === 'applied',
    `default engine: a link to a missing node is Left "${fromPure[0]}", applySetup throws, and a setup with no force along the links applies`);
  check(fromPure.every((value, i) => value === fromWasm[i]), `WASM engine: the same three outcomes, word for word ("${fromWasm[1]}")`);
}
{
  // The setup's params are the simulation's, whatever it was created with.
  const brisk = Setup.setupWithParams('brisk')([build(manyBody('charge'), Setup.withStrength(fixed(-20))), build(positionX('x')), build(positionY('y'))])({ alpha: 0.7, alphaMin: 0.001, alphaDecay: 0.05, alphaTarget: 0.3, velocityRetention: 0.9 });
  const sim = PureSim.create({ alphaMin: 0.001, alphaDecay: 0.5, alphaTarget: 0, velocityRetention: 0.1 })(); // none of this should survive the setup
  PureSim.setNodes(makeNodes())(sim)();
  Setup.applySetup(brisk)(sim)();
  const wasm = WasmSetup.create(makeNodes())([])();
  WasmSetup.applySetup(brisk)(wasm)();
  let alphas = [];
  for (let tick = 1; tick <= 3; tick++) {
    alphas = [PureSim.tick(sim)(), WasmSetup.tick(wasm)()];
    const pure = new Map(sim.nodes.value.map((d) => [d.id, [d.x, d.y]])), kernel = new Map(WasmSetup.getNodes(wasm)().map((d) => [d.id, [d.x, d.y]]));
    report('params from the setup, not from create', tick, compare(pure, kernel), tick <= 2);
  }
  check(Math.abs(alphas[0] - alphas[1]) < 1e-6 && Math.abs(alphas[0] - 0.64295) < 1e-5, `alpha, alphaDecay and alphaTarget are the setup's on both: after three ticks alpha is ${alphas[0].toFixed(6)} and ${alphas[1].toFixed(6)}`);
}
{
  // A pin set on a node between ticks (what a drag does), and cleared again.
  const setup = Setup.setupWithParams('pinned')([build(manyBody('charge'))])(params);
  const engines = { 'default engine': pureEngine(setup, makeNodes(), []), 'WASM engine': wasmEngine(setup, makeNodes(), []) };
  for (const [name, engine] of Object.entries(engines)) {
    engine.tick(); engine.tick();
    const before = { ...engine.nodes()[20] };
    engine.nodes()[20].fx = before.x + 25; engine.nodes()[20].fy = before.y - 40;
    engine.tick();
    const held = { ...engine.nodes()[20] };
    engine.tick();
    engine.nodes()[20].fx = null; engine.nodes()[20].fy = null;
    engine.tick();
    const freed = engine.nodes()[20];
    check(Math.abs(held.x - (before.x + 25)) < 1e-4 && Math.abs(held.y - (before.y - 40)) < 1e-4 && held.vx === 0 && (Math.abs(freed.x - held.x) > 1e-3 || Math.abs(freed.y - held.y) > 1e-3),
      `${name}: a pin set on a node between ticks holds it there, and clearing it lets it go`);
  }
}
{
  // Snapshot and restore on the WASM engine carry the pins with them.
  const setup = Setup.setupWithParams('snap')([build(manyBody('charge'))])(params);
  const sim = WasmSetup.create(makeNodes())([])();
  WasmSetup.applySetup(setup)(sim)();
  WasmSetup.tick(sim)();
  const node = WasmSetup.getNodes(sim)()[30];
  node.fx = node.x + 10; node.fy = node.y + 10;
  const pin = [node.fx, node.fy];
  const recorded = WasmSetup.snapshot(sim)();
  WasmSetup.getNodes(sim)()[30].fx = null; WasmSetup.getNodes(sim)()[30].fy = null;
  WasmSetup.tick(sim)(); WasmSetup.tick(sim)();
  const restored = WasmSetup.restore(recorded)(sim)();
  WasmSetup.tick(sim)(); WasmSetup.tick(sim)();
  const after = WasmSetup.getNodes(sim)()[30];
  check(restored && Math.abs(after.x - pin[0]) < 1e-4 && Math.abs(after.y - pin[1]) < 1e-4 && after.fx != null, 'WASM engine: a pin set on a node is in the snapshot, and comes back with it');
}

if (failures.length) {
  console.error(`\n${failures.length} FAILED`);
  for (const failure of failures) console.error(`  ${failure}`);
  process.exit(1);
}
console.error('\nthe two engines agree on what the Setup means');
