// Hylograph.Simulation.Stepped: one handle over both engines, meaning the
// same thing on each.
//
//   spago build && node test/stepped.mjs
//
// For each engine: the nodes are values (the caller's records are never
// written, the records returned are new after every tick), step, the
// measures, pins, retarget, applySetup taking every parameter, and a
// snapshot that restores exactly and by itself. Then the two engines against
// each other on the options added on 2026-11-18 (who a rectangle collides
// with, held rectangles, the squeeze, rectangles kept in a region, a hard
// directional constraint), to float width.
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const output = join(here, '../output');
const kernel = join(here, '../../../purescript-hylograph-showcases/wasm-force-demo/pkg/force_kernel.js');
const ps = (module) => import(pathToFileURL(join(output, module, 'index.js')));
const Setup = await ps('Hylograph.ForceEngine.Setup');
const Stepped = await ps('Hylograph.Simulation.Stepped');
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
const just = Maybe.Just.create, nothing = Maybe.Nothing.value;
const node = (id, x, y, extra = {}) => ({ id, x, y, vx: 0, vy: 0, fx: null, fy: null, ...extra });
const same = (a, b) => a.length === b.length && a.every((d, i) => Object.is(d.x, b[i].x) && Object.is(d.y, b[i].y) && Object.is(d.vx, b[i].vx) && Object.is(d.vy, b[i].vy) && d.fx === b[i].fx && d.fy === b[i].fy);
const worst = (a, b) => Math.max(...a.map((d, i) => Math.max(Math.abs(d.x - b[i].x), Math.abs(d.y - b[i].y))));
const engines = [['default engine', Stepped.Default.value], ['Rust engine', Stepped.Rust.value]];
const make = (engine, setup, nodes, links = []) => quietly(() => Stepped.create(engine)(setup)(nodes)(links)());

const cloud = (n) => Array.from({ length: n }, (_, i) => node(i + 100, (i % 9) * 14 - 50 + (i % 4), Math.floor(i / 9) * 12 - 40 + (i % 3), { r: 3 + (i % 3), w: i % 4 === 0 ? 3 : 14 + (i % 5) * 4 }));
const ring = (n) => Array.from({ length: n }, (_, i) => ({ source: 100 + i, target: 100 + ((i + 1) % n) }));
const params = (extra = {}) => ({ alpha: 1, alphaMin: 0.001, alphaDecay: 0.02, alphaTarget: 0, velocityRetention: 0.6, ...extra });
const home = (d) => just({ x: (d.id - 100) * 6 - 100, y: ((d.id - 100) % 5) * 20 });
const physics = (extra) => Setup.setupWithParams('physics')([
  build(Setup.manyBody('charge'), Setup.withStrength(Setup.static(-20))),
  build(Setup.link('links'), Setup.withDistance(Setup.static(25))),
  build(Setup.towards('home')(home), Setup.withStrength(Setup.static(0.05))),
  build(Setup.collide('bump'), Setup.withRadius(Setup.dynamic((d) => d.r))),
])(params(extra));

for (const [name, engine] of engines) {
  // --- the nodes are values --------------------------------------------------
  const given = cloud(30);
  const before = JSON.stringify(given);
  const sim = make(engine, physics(), given, ring(30));
  const first = sim.step();
  const second = sim.step();
  check(JSON.stringify(given.map(({ index, ...rest }) => rest)) === before || JSON.stringify(given) === before, `${name}: create and step write nothing into the caller's records`);
  check(first !== second && first[0] !== second[0] && first[0] !== given[0] && first[0].x !== second[0].x, `${name}: step returns new records every tick`);
  check(sim.nodes() === second && sim.tick() === 2 && second[3].r === given[3].r, `${name}: nodes is what the last step returned; tick counts; the nodes keep their own fields`);

  // --- snapshot, restore -------------------------------------------------------
  for (let t = 0; t < 38; t++) sim.step();
  const moment = sim.snapshot();
  const atMoment = sim.nodes().map((d) => ({ ...d }));
  check(Stepped.snapshotTick(moment) === 40 && Stepped.snapshotLinks(moment).length === 30 && same(Stepped.snapshotNodes(moment), atMoment) && Stepped.snapshotAlpha(moment) === sim.alpha(), `${name}: a snapshot carries its tick (40), the links, the nodes and alpha`);
  const followed = []; for (let t = 0; t < 30; t++) followed.push(sim.step().map((d) => ({ ...d })));
  // Wander off: other forces, other parameters, a pin, new targets.
  sim.applySetup(Setup.setupWithParams('other')([build(Setup.center('middle')), build(Setup.towards('home')(home))])(params({ velocityRetention: 0.9, alphaDecay: 0.1 })))();
  sim.pin(103)(just({ x: 500, y: 500 }))();
  sim.retarget('home')((d) => just({ x: 0, y: 0 }))();
  for (let t = 0; t < 25; t++) sim.step();
  const back = sim.restore(moment)();
  check(same(back, atMoment) && sim.tick() === 40 && sim.alpha() === Stepped.snapshotAlpha(moment), `${name}: restore puts the nodes, the tick and alpha back`);
  check(sim.settled({ step: 1e9, acceleration: 1e9 })() === false, `${name}: settled is false straight after a restore`);
  let exact = true; for (let t = 0; t < 30; t++) exact = exact && same(sim.step(), followed[t]);
  check(exact, `${name}: the 30 ticks after a restore are the 30 that followed the snapshot, bit for bit, with the forces, parameters, pin and targets changed in between`);
  check(sim.tick() === 70, `${name}: and the tick number goes on from there`);
  // A second handle cannot take the other engine's snapshot, but a restore twice over is fine.
  sim.restore(moment)(); let again = true; for (let t = 0; t < 30; t++) again = again && same(sim.step(), followed[t]);
  check(again, `${name}: and again from the same snapshot`);
  sim.free();

  // --- pins --------------------------------------------------------------------
  {
    const sim = make(engine, physics(), cloud(12), ring(12));
    sim.pin(104)(just({ x: 77, y: -33 }))();
    let held = true; for (let t = 0; t < 5; t++) { const now = sim.step(); held = held && now[4].x === 77 && now[4].y === -33 && now[4].vx === 0; }
    check(held, `${name}: pin holds the node with that id where it is told`);
    sim.pin(104)(nothing)();
    const free = sim.step();
    check(free[4].fx == null && (free[4].x !== 77 || free[4].y !== -33), `${name}: and lets it go`);
    // A pin written on the records last returned, as a drag does.
    const latest = sim.nodes(); latest[7].fx = -200; latest[7].fy = 10;
    const dragged = sim.step();
    check(dragged[7].x === -200 && dragged[7].y === 10, `${name}: a pin written on the latest records is read by the next step`);
    sim.free();
  }

  // --- retarget ----------------------------------------------------------------
  {
    const springs = Setup.setupWithParams('s')([build(Setup.towards('home')(home), Setup.withStrength(Setup.static(0.3)))])(params({ alphaDecay: 0, alphaTarget: 1 }));
    const sim = make(engine, springs, cloud(10));
    check(sim.retarget('nowhere')(home)() === false, `${name}: retarget says no where the setup has no Towards force of that name`);
    check(sim.retarget('home')((d) => just({ x: 300, y: 300 }))() === true, `${name}: and yes where it has`);
    for (let t = 0; t < 80; t++) sim.step();
    check(sim.nodes().every((d) => Math.abs(d.x - 300) < 0.5 && Math.abs(d.y - 300) < 0.5), `${name}: the nodes go to the new targets`);
    check(sim.settled({ step: 0.05, acceleration: 0.05 })() && sim.maxDisplacement() < 0.05 && sim.maxAcceleration() < 0.05 && sim.kineticEnergy() < 1, `${name}: and are settled there, by all three measures`);
    check(sim.cooled() === false, `${name}: though not cooled, alpha being held at 1`);
    sim.free();
  }

  // --- advance, reheat, reradius ------------------------------------------------
  {
    const one = make(engine, physics(), cloud(30), ring(30)), many = make(engine, physics(), cloud(30), ring(30));
    for (let t = 0; t < 37; t++) one.step();
    const together = many.advance(37)();
    check(same(together, one.nodes()) && many.tick() === 37 && Object.is(many.alpha(), one.alpha()), `${name}: advance 37 is 37 steps, to the bit, and counts them`);
    check(many.advance(0)() === many.nodes() && many.advance(-3)() === many.nodes() && many.tick() === 37, `${name}: advance by nothing, or by less, makes no tick`);
    // past alphaMin, where the handle's alpha rule matters: 400 ticks cool below 0.001
    for (let t = 0; t < 400; t++) one.step();
    many.advance(400)();
    check(same(many.nodes(), one.nodes()) && one.alpha() === 0 && many.alpha() === 0, `${name}: and still the same once alpha has fallen below alphaMin (both read 0)`);

    const atRest = one.nodes(), ticksBefore = one.tick();
    one.reheat();
    check(one.alpha() === 1 && one.tick() === ticksBefore && same(one.nodes(), atRest) && one.cooled() === false, `${name}: reheat puts alpha back to 1 and changes nothing else`);
    const moved = worst(one.step(), atRest);
    check(moved > 0, `${name}: and the forces push again (a node moved ${moved.toExponential(1)} in the next tick)`);
    one.free(); many.free();

    const pair = () => [node(0, -1, 0, { r: 2 }), node(1, 1, 0, { r: 2 })];
    const bump = Setup.setupWithParams('b')([build(Setup.collide('bump'), Setup.withRadius(Setup.dynamic((d) => d.r)))])(params({ alphaDecay: 0, alphaTarget: 1 }));
    const sim = make(engine, bump, pair());
    for (let t = 0; t < 200; t++) sim.step();
    const small = Math.abs(sim.nodes()[1].x - sim.nodes()[0].x);
    check(sim.reradius('nowhere')(() => 9)() === false, `${name}: reradius says no where the setup has no such force of that name`);
    check(sim.reradius('bump')(() => 30)() === true, `${name}: and yes where it has`);
    for (let t = 0; t < 200; t++) sim.step();
    const large = Math.abs(sim.nodes()[1].x - sim.nodes()[0].x);
    check(small >= 4 - 1e-3 && small < 60 && large >= 60 - 1e-3, `${name}: two nodes of radius 2 rest ${small.toFixed(3)} apart, and at radius 30, ${large.toFixed(3)}`);
    const kept = sim.snapshot();
    sim.reradius('bump')(() => 2)();
    sim.restore(kept)();
    for (let t = 0; t < 50; t++) sim.step();
    const after = Math.abs(sim.nodes()[1].x - sim.nodes()[0].x);
    check(after >= 60 - 1e-3, `${name}: a snapshot carries the radii it was taken with (${after.toFixed(3)} apart after a restore)`);
    sim.free();
  }

  // --- relink ---------------------------------------------------------------------
  {
    const line = () => [node(0, 0, 0), node(1, 100, 0), node(2, 200, 0), node(3, 300, 0)];
    const springs = Setup.setupWithParams('l')([build(Setup.link('links'), Setup.withDistance(Setup.static(20)), Setup.withStrength(Setup.static(0.5)))])(params({ alphaDecay: 0.02 }));
    const gap = (nodes, a, b) => Math.abs(nodes[b].x - nodes[a].x);
    const sim = make(engine, springs, line(), [{ source: 0, target: 1 }]);
    for (let t = 0; t < 30; t++) sim.step();
    const linked = gap(sim.nodes(), 0, 1), free = gap(sim.nodes(), 2, 3);
    const alphaBefore = sim.alpha(), ticksBefore = sim.tick(), nodesBefore = sim.nodes();
    sim.relink([{ source: 2, target: 3 }])();
    check(Object.is(sim.alpha(), alphaBefore) && sim.tick() === ticksBefore && same(sim.nodes(), nodesBefore) && sim.links().length === 1 && sim.links()[0].source === 2, `${name}: relink changes the links and nothing else (alpha still ${alphaBefore.toFixed(4)}, tick ${ticksBefore}, nodes where they were)`);
    for (let t = 0; t < 30; t++) sim.step();
    const nowFree = gap(sim.nodes(), 0, 1), nowLinked = gap(sim.nodes(), 2, 3);
    check(linked < 60 && free === 100 && nowLinked < 60 && Math.abs(nowFree - linked) < 1e-3, `${name}: the spring is where the link is (0-1 drew in to ${linked.toFixed(1)} and then stayed; 2-3, free at ${free}, drew in to ${nowLinked.toFixed(1)})`);
    let refused = false;
    try { quietly(() => sim.relink([{ source: 0, target: 99 }])()); } catch (problem) { refused = /node not found: 99/.test(String(problem)); }
    check(refused && sim.links().length === 1 && sim.links()[0].source === 2, `${name}: a link to a node that is not there is refused, and the links are left as they were`);
    const kept = sim.snapshot();
    sim.relink([])();
    sim.restore(kept)();
    check(sim.links().length === 1 && sim.links()[0].target === 3, `${name}: a snapshot carries the links it was taken with`);
    sim.free();
  }

  // --- applySetup takes every parameter ------------------------------------------
  {
    const none = (extra) => Setup.setupWithParams('none')([])(params(extra));
    const sim = make(engine, none({ velocityRetention: 0.5, alphaDecay: 0.5, alphaTarget: 0, alphaMin: 0.3 }), [node(0, 0, 0, { vx: 10 })]);
    const a = sim.step();
    check(Math.abs(a[0].x - 5) < 1e-5 && Math.abs(sim.alpha() - 0.5) < 1e-6 && sim.cooled() === false, `${name}: the setup's parameters are the simulation's (retention 0.5 moved a velocity of 10 by ${a[0].x}; alpha ${sim.alpha()})`);
    sim.applySetup(none({ alpha: 0.8, velocityRetention: 0.1, alphaDecay: 0.25, alphaTarget: 0.4, alphaMin: 0.9 }))();
    const b = sim.step();
    check(Math.abs(b[0].x - 5.5) < 1e-5 && sim.alpha() === 0 && sim.cooled() === true, `${name}: applySetup changes every one of them, not just alpha (retention 0.1: now at ${b[0].x}; alpha 0.8 cools to 0.7, under the new alphaMin of 0.9, so it is ${sim.alpha()}: cooled)`);
    sim.applySetup(none({ alpha: 0.8, velocityRetention: 0.1, alphaDecay: 0.25, alphaTarget: 0.4, alphaMin: 0.001 }))(); sim.step();
    check(Math.abs(sim.alpha() - 0.7) < 1e-6, `${name}: alpha 0.8, decay 0.25 toward a target of 0.4, is ${sim.alpha()} after a tick`);
    sim.free();
  }

  // --- a link to nowhere ---------------------------------------------------------
  {
    let thrown = null;
    try { make(engine, physics(), cloud(5), [{ source: 100, target: 999 }]); } catch (problem) { thrown = String(problem.message ?? problem); }
    check(thrown !== null && thrown.includes('node not found: 999'), `${name}: create throws on a link to a node that is not there (${thrown})`);
  }
}

// --- the two engines, on the options added with the handle ---------------------
{
  const dots = (d) => (d.id - 100) % 4 === 0;
  const labels = Setup.setupWithParams('labels')([
    build(Setup.towards('home')((d) => (dots(d) ? nothing : home(d))), Setup.withStrength(Setup.static(0.2))),
    build(Setup.rectCollide('apart'), Setup.withHalfWidth(Setup.dynamic((d) => d.w)), Setup.withHalfHeight(Setup.static(5)), Setup.withIterations(4), Setup.withThroughSqueeze(true)),
    build(Setup.rectCollide('zz off the dots'), Setup.withHalfWidth(Setup.dynamic((d) => d.w + 1)), Setup.withHalfHeight(Setup.static(6)), Setup.betweenGroups((d) => just(dots(d) ? 0 : 1)), Setup.withFilter((d) => d.id % 7 !== 0)),
    build(Setup.contain('frame')(new Setup.RectRegion({ x0: -120, y0: -80, x1: 140, y1: 110 })), Setup.withHalfWidth(Setup.dynamic((d) => d.w)), Setup.withHalfHeight(Setup.static(5))),
  ])(params({ alphaDecay: 0.05 }));
  const start = () => cloud(40).map((d) => (dots(d) ? { ...d, fx: d.x, fy: d.y } : d));
  const a = make(engines[0][1], labels, start()), b = make(engines[1][1], labels, start());
  const one = worst(a.step(), b.step());
  check(one < 1e-3, `labels (rect collide through, between groups, filter, pins held, rectangles contained): the engines agree after one tick to ${one.toExponential(1)}`);
  let inside = true, still = true;
  for (let t = 0; t < 200; t++) for (const sim of [a, b]) { const now = sim.step(); inside = inside && now.every((d) => dots(d) || (d.x - d.w >= -120.001 && d.x + d.w <= 140.001 && d.y - 5 >= -80.001 && d.y + 5 <= 110.001)); still = still && now.every((d, i) => !dots(d) || (d.x === start()[i].x && d.y === start()[i].y)); }
  check(inside, 'on both, every rectangle stays inside the frame by its half-extents at every tick');
  check(still, 'on both, a pinned rectangle is never moved by a collision');
  a.free(); b.free();

  const tree = Array.from({ length: 39 }, (_, i) => ({ source: 100 + Math.floor(i / 3), target: 101 + i }));
  const downhill = (hard) => Setup.setupWithParams('downhill')([
    build(Setup.manyBody('charge'), Setup.withStrength(Setup.static(-240)), Setup.withTheta(0)),
    build(Setup.link('links'), Setup.withDistance(Setup.static(30))),
    build(Setup.directional('zz downhill'), Setup.withGap(Setup.static(40)), Setup.withHardLimit(hard)),
  ])(params());
  const smallest = (nodes) => Math.min(...tree.map((l) => nodes[l.target - 100].y - nodes[l.source - 100].y));
  for (const [name, engine] of engines) {
    const soft = make(engine, downhill(false), cloud(40), tree), hard = make(engine, downhill(true), cloud(40), tree);
    let softLeast = Infinity, hardLeast = Infinity;
    for (let t = 0; t < 300; t++) { softLeast = Math.min(softLeast, t > 50 ? smallest(soft.step()) : (soft.step(), Infinity)); hardLeast = Math.min(hardLeast, smallest(hard.step())); }
    check(hardLeast >= 40 - 1e-3 && softLeast < 40 - 1, `${name}: a hard directional constraint keeps the gap asked for at every tick (least ${hardLeast.toFixed(4)} of 40; soft, after 50 ticks, as little as ${softLeast.toFixed(2)})`);
    soft.free(); hard.free();
  }
  const c = make(engines[0][1], downhill(true), cloud(40), tree), d = make(engines[1][1], downhill(true), cloud(40), tree);
  const two = worst(c.step(), d.step());
  check(two < 1e-3, `hard directional: the engines agree after one tick to ${two.toExponential(1)}`);
}

{
  const line = () => [node(0, 0, 0), node(1, 100, 0), node(2, 200, 0), node(3, 300, 0)];
  const springs = Setup.setupWithParams('l')([build(Setup.link('links'), Setup.withDistance(Setup.static(20)), Setup.withStrength(Setup.static(0.5)))])(params());
  const a = make(engines[0][1], springs, line(), [{ source: 0, target: 1 }]), b = make(engines[1][1], springs, line(), [{ source: 0, target: 1 }]);
  for (let t = 0; t < 5; t++) { a.step(); b.step(); }
  a.relink([{ source: 1, target: 2 }, { source: 2, target: 3 }])(); b.relink([{ source: 1, target: 2 }, { source: 2, target: 3 }])();
  let apart = 0; for (let t = 0; t < 5; t++) apart = worst(a.step(), b.step());
  check(apart < 1e-3 && Math.abs(a.alpha() - b.alpha()) < 1e-6, `relink: the engines agree five ticks after it to ${apart.toExponential(1)}, alpha ${a.alpha().toFixed(5)} and ${b.alpha().toFixed(5)}`);
  a.free(); b.free();
}

console.log(failed === 0 ? '\nall passed' : `\n${failed} FAILED`);
process.exit(failed === 0 ? 0 : 1);
