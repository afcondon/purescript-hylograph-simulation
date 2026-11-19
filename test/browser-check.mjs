// Does the library work in a real browser, with no D3 behind it?
//
//   (cd ../purescript-hylograph-demos && make simulation)   # bundle the Force Playground
//   spago build && node test/browser-check.mjs [port]
//
// Two pages in headless Chrome.
//
// A. The Force Playground demo (purescript-hylograph-demos/simulation), as
//    bundled, unmodified:
//      1. the bundle contains no D3 module;
//      2. nodes move;
//      3. dragging a node moves it with the pointer, and the others respond;
//      4. two loads of the page come to rest in identical positions. The demo
//         draws its random network with Math.random, so the page is given a
//         seeded Math.random first; the simulation itself is then also shown
//         not to call it (the D3 engine did, to separate coincident nodes).
//
// B. A page made here from the compiled library, for the three drags the
//    library itself provides (the demo's own drag comes from
//    hylograph-selection): attachDrag, attachPinningDrag, attachGroupDrag.
//
// C. The high-level handle (`Hylograph.Simulation.runSimulation`) on each
//    engine in turn, default and WASM, in a page made here: after the
//    simulation has run to completion, dragging a node still moves the
//    picture; `pinNodes`, `unpinNodes` and `interpolatePositions` do on one
//    engine what they do on the other.
import { createServer } from 'node:http';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const demo = join(here, '../../purescript-hylograph-demos/simulation/public');
const films = join(homedir(), 'work/afc-work/hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = Number(process.argv[2] || 4186);

const failures = [];
const check = (ok, what) => { console.log(`${ok ? 'ok    ' : 'FAILED'} ${what}`); if (!ok) failures.push(what); };

// B's page: the library's drags over three circles and one group.
const scratch = mkdtempSync(join(tmpdir(), 'hylograph-drag-'));
writeFileSync(join(scratch, 'entry.js'), `
  import * as Sim from ${JSON.stringify(join(here, '../output/Hylograph.ForceEngine.Simulation/index.js'))};
  const svg = document.querySelector('svg');
  const make = (tag, attrs, parent) => { const el = document.createElementNS('http://www.w3.org/2000/svg', tag); for (const [k, v] of Object.entries(attrs)) el.setAttribute(k, v); parent.appendChild(el); return el; };
  const nodes = [0, 1, 2, 3].map((id) => ({ id, x: 100 + id * 120, y: 150, vx: 0, vy: 0, fx: null, fy: null }));
  const sim = Sim.create({ alphaMin: 0.001, alphaDecay: 0.02, alphaTarget: 0, velocityRetention: 0.6 })();
  Sim.setNodes(nodes)(sim)();
  const zoom = make('g', { id: 'zoom', transform: 'translate(40,20) scale(2)' }, svg);
  const circles = nodes.slice(0, 3).map((node) => { const el = make('circle', { r: 12, cx: node.x, cy: node.y }, zoom); el.__data__ = node; return el; });
  const pack = make('g', { class: 'module-pack', transform: 'translate(' + nodes[3].x + ',' + nodes[3].y + ')' }, zoom);
  pack.__data__ = nodes[3];
  make('circle', { r: 14 }, pack);
  Sim.attachDrag([circles[0]])(sim)();
  Sim.attachPinningDrag([circles[1]])(sim)();
  Sim.attachGroupDrag([pack])('#zoom')(sim)();
  Sim.onTick(() => { circles.forEach((el) => { el.setAttribute('cx', el.__data__.x); el.setAttribute('cy', el.__data__.y); }); pack.setAttribute('transform', 'translate(' + nodes[3].x + ',' + nodes[3].y + ')'); })(sim)();
  window.nodes = nodes; window.sim = sim; window.running = () => Sim.isRunning(sim)();
`);
execFileSync('esbuild', [join(scratch, 'entry.js'), '--bundle', '--format=iife', `--outfile=${join(scratch, 'drag.js')}`, '--log-level=warning']);
// C's page: six nodes, a `towards` force to a row of slots, the handle on
// window. `?engine=wasm` picks the kernel.
writeFileSync(join(scratch, 'handle-entry.js'), `
  import * as Simulation from ${JSON.stringify(join(here, '../output/Hylograph.Simulation/index.js'))};
  import * as Setup from ${JSON.stringify(join(here, '../output/Hylograph.ForceEngine.Setup/index.js'))};
  import * as Emitter from ${JSON.stringify(join(here, '../output/Hylograph.Simulation.Emitter/index.js'))};
  import * as Pointer from ${JSON.stringify(join(here, '../output/Hylograph.Interaction.Pointer/index.js'))};
  import * as Maybe from ${JSON.stringify(join(here, '../output/Data.Maybe/index.js'))};
  import { initWasm_ } from ${JSON.stringify(join(here, '../output/Hylograph.ForceEngine.WASM/foreign.js'))};
  const wasm = new URLSearchParams(location.search).get('engine') === 'wasm';
  const svg = document.querySelector('svg');
  const slot = (d) => Maybe.Just.create({ x: 100 + d.id * 80, y: 200 });
  const nodes = [0, 1, 2, 3, 4, 5].map((id) => ({ id, x: 120 + id * 70, y: 120 + (id % 2) * 60, vx: 0, vy: 0, fx: null, fy: null }));
  const setup = Setup.setupWithParams('row')([Setup.withStrength(Setup.static(0.3))(Setup.towards('home')(slot))])({ alpha: 1, alphaMin: 0.001, alphaDecay: 0.08, alphaTarget: 0, velocityRetention: 0.6 });
  const start = () => {
    const { handle, events } = Simulation.runSimulation({ engine: wasm ? Simulation.WASM.value : Simulation.Pure.value, setup, nodes, links: [], container: 'svg', alphaMin: 0.001 })();
    const circles = nodes.map(() => { const el = document.createElementNS('http://www.w3.org/2000/svg', 'circle'); el.setAttribute('r', 14); svg.appendChild(el); return el; });
    const draw = () => handle.getNodes().forEach((d, i) => { circles[i].__data__ = d; circles[i].setAttribute('cx', d.x); circles[i].setAttribute('cy', d.y); });
    window.ticks = 0; window.completed = 0;
    Emitter.subscribe(events)((event) => () => { if (event instanceof Emitter.Tick) { window.ticks++; draw(); } if (event instanceof Emitter.Completed) window.completed++; })();
    draw();
    circles.forEach((el) => Pointer.attachSimulationDrag(el)(handle.reheat)());
    window.handle = handle; window.draw = draw;
    window.state = () => handle.getNodes().map((d) => ({ id: d.id, x: d.x, y: d.y, fx: d.fx, fy: d.fy }));
    window.ready = true;
  };
  if (wasm) initWasm_('/pkg/force_kernel.js')((error) => { window.failed = String(error); }, start); else start();
`);
execFileSync('esbuild', [join(scratch, 'handle-entry.js'), '--bundle', '--format=esm', `--outfile=${join(scratch, 'handle.js')}`, '--log-level=warning']);
writeFileSync(join(scratch, 'handle.html'), '<!doctype html><body style="margin:0"><svg width="900" height="500"></svg><script type="module" src="handle.js"></script></body>');
const kernel = join(homedir(), 'work/afc-work/purescript-hylograph-showcases/wasm-force-demo/pkg');
writeFileSync(join(scratch, 'index.html'), '<!doctype html><body style="margin:0"><svg width="1200" height="700"></svg><script src="drag.js"></script></body>');

const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json', '.wasm': 'application/wasm' };
const server = createServer((request, response) => {
  const path = normalize(decodeURIComponent(new URL(request.url, 'http://x').pathname));
  const root = path.startsWith('/drag/') ? scratch : path.startsWith('/pkg/') ? kernel : demo;
  const file = path.startsWith('/drag/') ? path.slice(5) : path.startsWith('/pkg/') ? path.slice(4) : path;
  try {
    const body = readFileSync(join(root, file === '/' ? 'index.html' : file));
    response.writeHead(200, { 'content-type': types[extname(file)] || 'text/html' }).end(body);
  } catch { response.writeHead(404).end(); }
});
await new Promise((resolve, reject) => server.once('error', reject).listen(PORT, '127.0.0.1', resolve));

const browser = await puppeteer.launch({ executablePath: CHROME, headless: true });
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

try {
  // -------------------------------------------------------------------------
  // A. The Force Playground
  // -------------------------------------------------------------------------
  const bundle = readFileSync(join(demo, 'bundle.js'), 'utf8');
  check(!/node_modules\/d3-|d3-force|d3-drag|d3-selection|d3-quadtree/.test(bundle) && bundle.includes('Hylograph.Forces'),
    `the demo's bundle (${(bundle.length / 1024).toFixed(0)} KB) names no D3 module, and contains hylograph-forces`);

  // A page with a seeded Math.random that counts its calls.
  const open = async () => {
    const page = await browser.newPage();
    await page.setViewport({ width: 1400, height: 900 });
    const problems = [];
    page.on('pageerror', (error) => problems.push(String(error)));
    await page.evaluateOnNewDocument(() => {
      let seed = 20261118, calls = 0;
      Math.random = () => { calls++; seed = (seed + 0x6d2b79f5) | 0; let t = Math.imul(seed ^ (seed >>> 15), 1 | seed); t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t; return ((t ^ (t >>> 14)) >>> 0) / 4294967296; };
      window.randomCalls = () => calls;
    });
    await page.goto(`http://127.0.0.1:${PORT}/`);
    await page.waitForSelector('#network-nodes circle', { timeout: 20000 });
    return { page, problems };
  };
  const positions = (page) => page.evaluate(() => Array.from(document.querySelectorAll('#network-nodes circle'), (el) => [el.getAttribute('cx'), el.getAttribute('cy')]));
  const model = (page) => page.evaluate(() => Array.from(document.querySelectorAll('#network-nodes circle'), (el) => { const d = el.__data__.node || el.__data__; return [d.id, d.x, d.y]; }).sort((a, b) => a[0] - b[0]));
  const atRest = async (page) => {
    let last = JSON.stringify(await model(page));
    for (let waited = 0; waited < 120000; waited += 400) {
      await sleep(400);
      const now = JSON.stringify(await model(page));
      if (now === last) return JSON.parse(now);
      last = now;
    }
    throw new Error('the simulation did not come to rest in two minutes');
  };

  const first = await open();
  const started = await positions(first.page);
  const callsAfterLoad = await first.page.evaluate(() => window.randomCalls());
  await sleep(600);
  const later = await positions(first.page);
  const moved = started.filter(([x, y], i) => later[i] && (x !== later[i][0] || y !== later[i][1])).length;
  check(started.length > 10 && moved > started.length / 2, `nodes move: ${moved} of ${started.length} circles changed place in 0.6 s`);

  const restFirst = await atRest(first.page);
  const callsAtRest = await first.page.evaluate(() => window.randomCalls());
  check(callsAtRest === callsAfterLoad, `the simulation never calls Math.random (${callsAfterLoad} calls to draw the network, ${callsAtRest - callsAfterLoad} while it ran to rest)`);

  // Drag the first circle 90 px right and 60 px down.
  const grab = await first.page.evaluate(() => { const el = document.querySelector('#network-nodes circle'); const box = el.getBoundingClientRect(); const d = el.__data__.node || el.__data__; return { x: box.left + box.width / 2, y: box.top + box.height / 2, id: d.id, nodeX: d.x, nodeY: d.y }; });
  const others = restFirst.filter(([id]) => id !== grab.id);
  await first.page.mouse.move(grab.x, grab.y);
  await first.page.mouse.down();
  for (let step = 1; step <= 15; step++) { await first.page.mouse.move(grab.x + step * 6, grab.y + step * 4); await sleep(30); }
  await sleep(300);
  const held = await first.page.evaluate((id) => { const el = Array.from(document.querySelectorAll('#network-nodes circle')).find((c) => (c.__data__.node || c.__data__).id === id); const box = el.getBoundingClientRect(); const d = el.__data__.node || el.__data__; return { x: box.left + box.width / 2, y: box.top + box.height / 2, fx: d.fx, fy: d.fy }; }, grab.id);
  const offBy = Math.hypot(held.x - (grab.x + 90), held.y - (grab.y + 60));
  check(held.fx != null && held.fy != null && offBy < 3, `drag: the node is pinned under the pointer while held (${offBy.toFixed(2)} px from it)`);
  await first.page.mouse.up();
  await sleep(500);
  const released = await first.page.evaluate((id) => { const d = Array.from(document.querySelectorAll('#network-nodes circle'), (c) => c.__data__.node || c.__data__).find((n) => n.id === id); return { fx: d.fx, fy: d.fy }; }, grab.id);
  const afterDrag = await model(first.page);
  const disturbed = others.filter(([id, x, y]) => { const now = afterDrag.find((n) => n[0] === id); return now && (now[1] !== x || now[2] !== y); }).length;
  check(released.fx == null && released.fy == null && disturbed > 0, `drag: released on pointer up, and the simulation reheated (${disturbed} other nodes moved)`);
  check(first.problems.length === 0, `the page reported nothing${first.problems.length ? ': ' + first.problems[0] : ''}`);
  await first.page.close();

  const second = await open();
  const restSecond = await atRest(second.page);
  const same = restFirst.length === restSecond.length && restFirst.every(([id, x, y], i) => id === restSecond[i][0] && Object.is(x, restSecond[i][1]) && Object.is(y, restSecond[i][2]));
  check(same, `a fresh page comes to rest in identical positions (${restFirst.length} nodes, compared to the last bit)`);
  await second.page.close();

  // -------------------------------------------------------------------------
  // B. The library's own drags
  // -------------------------------------------------------------------------
  const page = await browser.newPage();
  await page.setViewport({ width: 1200, height: 700 });
  await page.goto(`http://127.0.0.1:${PORT}/drag/index.html`);
  await page.waitForFunction(() => window.nodes);
  // The group is translate(40,20) scale(2): node (x, y) is on screen at (40 + 2x, 20 + 2y).
  const screen = (node) => [40 + 2 * node.x, 20 + 2 * node.y];
  const node = (i) => page.evaluate((k) => ({ ...window.nodes[k] }), i);
  const drag = async (i, dx, dy, during) => {
    const [x, y] = screen(await node(i));
    await page.mouse.move(x, y);
    await page.mouse.down();
    for (let step = 1; step <= 10; step++) { await page.mouse.move(x + (dx * step) / 10, y + (dy * step) / 10); await sleep(20); }
    await sleep(120);
    const held = await node(i);
    if (during) await during();
    await page.mouse.up();
    await sleep(120);
    return { held, after: await node(i), from: [x, y] };
  };
  const near = (a, b) => Math.abs(a - b) < 1.5;

  {
    const before = await node(0);
    const { held, after } = await drag(0, 100, 40);
    check(near(held.fx, before.x + 50) && near(held.fy, before.y + 20) && near(held.x, held.fx) && after.fx == null && after.fy == null,
      `attachDrag: follows the pointer in the group's own coordinates (moved ${(held.x - before.x).toFixed(1)}, ${(held.y - before.y).toFixed(1)} for 100, 40 px on screen at scale 2), released on pointer up`);
  }
  {
    const before = await node(1);
    const { held, after } = await drag(1, -60, 80);
    const pinned = near(after.fx, before.x - 30) && near(after.fy, before.y + 40);
    await sleep(200);
    const { after: again } = await drag(1, 0, 0);
    check(near(held.fx, before.x - 30) && pinned && again.fx == null && again.fy == null,
      'attachPinningDrag: stays pinned where it is dropped; pressing it again without moving unpins it');
  }
  {
    const before = await node(3);
    const { held, after } = await drag(3, 80, -30);
    check(near(held.fx, before.x + 40) && near(held.fy, before.y - 15) && after.fx == null,
      'attachGroupDrag: the group follows the pointer in the container\'s coordinates, released on pointer up');
  }
  check(await page.evaluate(() => window.nodes.every((d) => Number.isFinite(d.x) && Number.isFinite(d.y))), 'every node is still somewhere finite');
  await page.close();

  // -------------------------------------------------------------------------
  // C. The high-level handle, on each engine
  // -------------------------------------------------------------------------
  for (const engine of ['default', 'wasm']) {
    const page = await browser.newPage();
    await page.setViewport({ width: 900, height: 500 });
    const problems = [];
    page.on('pageerror', (error) => problems.push(String(error)));
    await page.goto(`http://127.0.0.1:${PORT}/drag/handle.html?engine=${engine}`);
    await page.waitForFunction(() => window.ready || window.failed, { timeout: 20000 });
    const label = `${engine} engine`;
    const state = () => page.evaluate(() => window.state());
    // Do something that heats the simulation, then wait for it to run to completion again.
    const untilCompleted = async (action) => {
      const before = await page.evaluate(() => window.completed);
      if (action) await page.evaluate(action);
      try { await page.waitForFunction((n) => window.completed > n, { timeout: 15000 }, action ? before : 0); return true; }
      catch { const seen = await page.evaluate(() => ({ ticks: window.ticks, completed: window.completed, alpha: window.handle.getAlpha() })); check(false, `${label}: a run did not complete (${JSON.stringify(seen)})`); return false; }
    };
    const close = (a, b, slack) => Math.abs(a - b) <= slack;

    await untilCompleted(null);
    const rested = await state();
    check(rested.every((d) => close(d.x, 100 + d.id * 80, 0.5) && close(d.y, 200, 0.5)), `${label}: runs to completion with every node at its slot`);

    // Drag node 2 after completion: the picture has to follow.
    const from = { x: rested[2].x, y: rested[2].y };
    await page.mouse.move(from.x, from.y);
    await page.mouse.down();
    for (let step = 1; step <= 10; step++) { await page.mouse.move(from.x + step * 7, from.y + step * 9); await sleep(30); }
    await sleep(400);
    const held = await page.evaluate(() => { const el = document.querySelectorAll('circle')[2]; return { cx: Number(el.getAttribute('cx')), cy: Number(el.getAttribute('cy')) }; });
    check(close(held.cx, from.x + 70, 1) && close(held.cy, from.y + 90, 1), `${label}: after completion, a dragged node follows the pointer on screen (${(held.cx - from.x).toFixed(1)}, ${(held.cy - from.y).toFixed(1)} of 70, 90)`);
    await page.mouse.up();
    await sleep(100);
    const let_go = await state();
    await untilCompleted(() => window.handle.reheat());
    const back = await state();
    check(let_go[2].fx == null && let_go[2].fy == null && close(back[2].x, 260, 0.5) && close(back[2].y, 200, 0.5), `${label}: released on pointer up, and with a reheat it goes back to its slot`);

    // pinNodes: every node pinned where it is, and a reheat moves nothing.
    const startPositions = Object.fromEntries(back.map((d) => [String(d.id), { x: d.x, y: d.y }]));
    const targetPositions = Object.fromEntries(back.map((d) => [String(d.id), { x: d.x + 40, y: d.y - 100 }]));
    await page.evaluate((a, b) => { window.handle.interpolatePositions(a)(b)(0.25)(); window.draw(); }, startPositions, targetPositions);
    const quarter = await state();
    check(quarter.every((d, i) => close(d.x, back[i].x + 10, 1e-3) && close(d.y, back[i].y - 25, 1e-3)), `${label}: interpolatePositions puts every node a quarter of the way`);
    await page.evaluate(() => window.handle.pinNodes());
    const pinned = await state();
    check(pinned.every((d, i) => d.fx != null && d.fy != null && close(d.fx, quarter[i].x, 1e-3) && close(d.fy, quarter[i].y, 1e-3)), `${label}: pinNodes pins every node where it is`);
    await untilCompleted(() => window.handle.reheat());
    const stillThere = await state();
    check(stillThere.every((d, i) => close(d.x, quarter[i].x, 1e-3) && close(d.y, quarter[i].y, 1e-3)), `${label}: pinned nodes stay put through a whole run, against the force`);
    await untilCompleted(() => { window.handle.unpinNodes(); window.handle.reheat(); });
    const freed = await state();
    check(freed.every((d) => d.fx == null && d.fy == null && close(d.x, 100 + d.id * 80, 0.5) && close(d.y, 200, 0.5)), `${label}: unpinNodes frees them, and the force takes them home`);
    check(problems.length === 0 && !(await page.evaluate(() => window.failed)), `${label}: the page reported nothing${problems.length ? ': ' + problems[0] : ''}`);
    await page.close();
  }
} finally {
  await browser.close();
  server.close();
}

if (failures.length) {
  console.error(`\n${failures.length} FAILED`);
  process.exit(1);
}
console.log('\nall checks passed');
