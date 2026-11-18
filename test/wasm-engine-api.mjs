// The WASM engine's PureScript API, driven from Node against the built kernel.
//
//   spago build && node test/wasm-engine-api.mjs
//
// Covers what was added on 2026-11-18: velocityRetention (and that the
// deprecated withVelocityDecay still means the same thing), step, retargetTo,
// reradius, maxAcceleration / settled, and a Collide that yields (filter,
// strength scaled by alpha). The kernel's own arithmetic is tested in the
// Rust crate; this checks that the PureScript reaches it.
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const output = join(here, '../output');
const kernel = join(here, '../../../purescript-hylograph-showcases/wasm-force-demo/pkg/force_kernel.js');
const ps = (module) => import(pathToFileURL(join(output, module, 'index.js')));
const Setup = await ps('Hylograph.ForceEngine.Setup');
const W = await ps('Hylograph.ForceEngine.Setup.WASM');
const PureSim = await ps('Hylograph.ForceEngine.Simulation');
const Maybe = await ps('Data.Maybe');

const realFetch = globalThis.fetch;
globalThis.fetch = async (url, ...rest) => {
  const href = String(url instanceof Request ? url.url : url);
  if (!href.startsWith('file:')) return realFetch(url, ...rest);
  return new Response(readFileSync(fileURLToPath(href)), { headers: { 'content-type': 'application/wasm' } });
};
const quietly = (run) => { const log = console.log; console.log = () => {}; try { return run(); } finally { console.log = log; } };
const { initWasm_ } = await import(pathToFileURL(join(output, 'Hylograph.ForceEngine.WASM/foreign.js')));
await new Promise((resolve, reject) => quietly(() => initWasm_(pathToFileURL(kernel).href)(reject, resolve)));

let failed = 0;
const check = (ok, what) => { console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`); if (!ok) failed++; };
const build = (force, ...modifiers) => modifiers.reduce((config, modify) => modify(config), force);
const just = Maybe.Just.create;
const params = (velocityRetention) => ({ alpha: 1, alphaMin: 0.001, alphaDecay: 0, alphaTarget: 1, velocityRetention });
const node = (id, x, y, extra = {}) => ({ id, x, y, vx: 0, vy: 0, fx: null, fy: null, ...extra });
const same = (a, b) => a.length === b.length && a.every((d, i) => d.x === b[i].x && d.y === b[i].y && d.vx === b[i].vx && d.vy === b[i].vy);
const simulation = (nodes, setup) => { const sim = W.create(nodes)([])(); W.applySetup(setup)(sim)(); return sim; };

// --- velocityRetention -------------------------------------------------------
{
  // A velocity of 10 with retention 0.7 moves the node 7.
  const sim = simulation([node(0, 0, 0, { vx: 10 })], Setup.setupWithParams('none')([])(params(0.7)));
  const [moved] = W.step(sim)();
  check(Math.abs(moved.x - 7) < 1e-5, `velocityRetention is the share kept: 10 * 0.7 moved the node ${moved.x}`);

  const byNewName = Setup.withVelocityRetention(0.3)(Setup.setup('a')([]));
  const byOldName = Setup.withVelocityDecay(0.3)(Setup.setup('a')([]));
  check(byNewName.params.velocityRetention === 0.3 && byOldName.params.velocityRetention === 0.3, 'withVelocityDecay (deprecated) sets the same field to the same number');
  check(Setup.defaultParams.velocityRetention === 0.4 && !('velocityDecay' in Setup.defaultParams), 'the default is unchanged (0.4 kept) and the old field is gone');

  // The default engine reads the same field the same way.
  const d3 = PureSim.create({ alphaMin: 0.001, alphaDecay: 0, alphaTarget: 1, velocityRetention: 0.7 })();
  const d3Nodes = [node(0, 0, 0, { vx: 10 })];
  PureSim.setNodes(d3Nodes)(d3)();
  quietly(() => PureSim.tick(d3)());
  check(Math.abs(d3.nodes.value[0].x - 7) < 1e-9, `the default engine keeps the same share: moved ${d3.nodes.value[0].x}`);
}

// --- step, retargetTo --------------------------------------------------------
const spread = (n) => Array.from({ length: n }, (_, i) => node(i, (i % 7) * 13 - 40, Math.floor(i / 7) * 11 - 30, { r: 3 + (i % 3) }));
const home = (d) => just({ x: d.id * 5, y: -d.id * 3 });
const springs = (retention = 0.85) => Setup.setupWithParams('s')([build(Setup.towards('home')(home), Setup.withStrength(Setup.static(0.02)))])(params(retention));
{
  const a = simulation(spread(40), springs()), b = simulation(spread(40), springs());
  let stepped, alike = true;
  for (let t = 0; t < 30; t++) { stepped = W.step(a)(); W.tick(b)(); alike = alike && same(stepped, W.getNodes(b)()); }
  check(alike && stepped.length === 40 && stepped[3].r === 3, 'step returns the nodes tick + getNodes would, with their own fields');

  const elsewhere = (d) => ({ x: -d.id * 4, y: d.id * 2 });
  const byFunction = W.retarget('home')((d) => just(elsewhere(d)))(a)();
  const byPoints = W.retargetTo('home')(spread(40).map(elsewhere))(b)();
  for (let t = 0; t < 30; t++) { W.step(a)(); W.step(b)(); }
  check(byFunction && byPoints && same(W.getNodes(a)(), W.getNodes(b)()), 'retargetTo with the points is retarget with a function');
  check(W.retargetTo('nope')([])(a)() === false, 'retargetTo refuses a name the setup does not have');
}

// --- nodes are values ---------------------------------------------------------
{
  const given = spread(40);
  const before = given.map((d) => ({ ...d }));
  const sim = simulation(given, springs());
  const first = W.step(sim)();
  const kept = first.map((d) => ({ x: d.x, y: d.y, vx: d.vx, vy: d.vy }));
  const read = W.getNodes(sim)();
  for (let t = 0; t < 20; t++) W.step(sim)();
  W.tick(sim)(); W.tickN(3)(sim)();
  const unchanged = (nodes, record) => nodes.every((d, i) => d.x === record[i].x && d.y === record[i].y && d.vx === record[i].vx && d.vy === record[i].vy);
  check(unchanged(first, kept) && unchanged(read, kept), 'the nodes a tick returned still say what they said after later ticks');
  check(unchanged(given, before), 'the nodes the simulation was created from are not written into');
  check(!unchanged(W.getNodes(sim)(), kept), '(and the simulation did move)');
}

// --- maxAcceleration, settled ------------------------------------------------
{
  const sim = simulation(spread(40), springs(0.9));
  const tolerance = { step: 0.01, acceleration: 0.01 * 0.02 };
  const far = () => Math.max(...W.getNodes(sim)().map((d) => Math.hypot(d.x - d.id * 5, d.y + d.id * 3)));
  check(W.settled(tolerance)(sim)() === false && W.maxAcceleration(sim)() === 0, 'not settled, and no acceleration to report, before two ticks');
  let turningPoints = 0, fooled = 0, settledAt = null;
  for (let t = 1; t <= 3000 && settledAt === null; t++) {
    W.step(sim)();
    if (W.maxDisplacement(sim)() < 0.5 && far() > 5) { turningPoints++; if (W.settled({ step: 0.5, acceleration: tolerance.acceleration })(sim)()) fooled++; }
    if (W.settled(tolerance)(sim)()) settledAt = t;
  }
  const bound = (tolerance.acceleration + 0.1 * (tolerance.step + tolerance.acceleration)) / (0.02 * 0.9);
  check(turningPoints >= 2 && fooled === 0, `at the ${turningPoints} ticks where nothing moved more than 0.5 but nodes were over 5 from rest, settled said no`);
  check(settledAt !== null && far() < bound, `settled at tick ${settledAt}, ${far().toFixed(4)} from the targets (bound ${bound.toFixed(4)})`);
}

// --- a Collide that yields ---------------------------------------------------
{
  const three = () => [node(0, 0, 0), node(1, 2.4, 3.2), node(2, 4.8, 6.4)];
  const collision = (...modifiers) => Setup.setupWithParams('c')([build(Setup.collide('bump'), Setup.withRadius(Setup.static(5)), ...modifiers)])({ ...params(1), alpha: 0.25, alphaTarget: 0.25 });
  const push = (setup, nodes = three()) => W.step(simulation(nodes, setup))().map((d) => Math.hypot(d.vx, d.vy));
  // Two circles of radius 5, 6 apart: overlap 4, half each.
  const two = () => [node(0, 0, 0), node(1, 3.6, 4.8)];
  const plain = push(collision(), two());
  const scaled = push(collision(Setup.withAlphaScaling(true)), two());
  check(scaled.every((v, i) => Math.abs(v - plain[i] * 0.25) < 1e-5) && Math.abs(plain[0] - 2) < 1e-5, `scaled by alpha (0.25) every push is a quarter: ${plain.map((v) => v.toFixed(3))} -> ${scaled.map((v) => v.toFixed(3))}`);
  const filtered = push(collision(Setup.withFilter((d) => d.id !== 1)));
  check(filtered[1] === 0 && Math.abs(filtered[0] - 1) < 1e-5 && Math.abs(filtered[2] - 1) < 1e-5, `a filtered-out node neither pushes nor is pushed: ${filtered.map((v) => v.toFixed(3))}`);

  const sim = simulation(three(), collision());
  const shrunk = W.reradius('bump')(() => 0.1)(sim)();
  const still = W.step(sim)().every((d) => d.vx === 0 && d.vy === 0);
  check(shrunk && still && W.reradius('nope')(() => 1)(sim)() === false, 'reradius replaces the radii without re-applying the setup');

  // The default engine runs a yielding Collide too (the D3 engine it replaced refused one).
  const accepts = (setup) => { const sim = PureSim.create({ alphaMin: 0.001, alphaDecay: 0, alphaTarget: 0, velocityRetention: 0.6 })(); PureSim.setNodes(three())(sim)(); try { Setup.applySetup(setup)(sim)(); PureSim.tick(sim)(); return sim.nodes.value.every((d) => Number.isFinite(d.x)); } catch (error) { return false; } };
  check(accepts(collision(Setup.withAlphaScaling(true))) && accepts(collision(Setup.withFilter(() => true))) && accepts(collision()), 'the default engine runs a yielding Collide, and a plain one');
}

// --- containEach ------------------------------------------------------------
{
  // Three nodes, all starting at the origin with a push to the right: one
  // confined to a circle round the origin, one to a circle elsewhere, one free.
  const regions = [new Setup.CircleRegion({ x: 0, y: 0, radius: 10 }), new Setup.CircleRegion({ x: 100, y: 50, radius: 5 })];
  const setup = Setup.setupWithParams('c')([Setup.containEach('pen')(regions)((d) => (d.pen === null ? Maybe.Nothing.value : just(d.pen)))])(params(1));
  const sim = simulation([node(0, 0, 0, { vx: 40, pen: 0 }), node(1, 0, 0, { vx: 40, pen: 1 }), node(2, 0, 0, { vx: 40, pen: null })], setup);
  let nodes; for (let t = 0; t < 5; t++) nodes = W.step(sim)();
  const within = (d, region) => Math.hypot(d.x - region.value0.x, d.y - region.value0.y) <= region.value0.radius + 1e-4;
  check(within(nodes[0], regions[0]) && within(nodes[1], regions[1]) && nodes[2].x > 150, `containEach keeps each node in its own region and leaves the rest free: x = ${nodes.map((d) => d.x.toFixed(1))}`);
}

console.log(failed ? `${failed} failed` : 'all passed');
process.exit(failed ? 1 : 0);
