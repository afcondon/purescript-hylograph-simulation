// How long does a tick take through the library's own loop, on each engine?
//
//   spago build && node test/timing.mjs [--runs 3] [--sizes 100,1000,10000] [--es <dir>]
//
// The same Setup (many-body -30 at theta 0.9, links at distance 30 with
// d3's degree strength, centring), the same graph as hylograph-forces-pure's
// timing (a ring with chords on d3's phyllotaxis spiral), 300 ticks:
//
//   default engine   Hylograph.ForceEngine.Simulation.tick, the forces in
//                    PureScript, nodes updated in place;
//   WASM engine      Hylograph.ForceEngine.Setup.WASM.tick: the kernel's
//                    tick and the copy of positions back into the nodes.
//
// `--es <dir>` adds the default engine as compiled by purs-backend-es into
// that directory (purs-backend-es build --corefn-dir output --output-dir <dir>).
//
// Measured as that package measures: 30 blocks of ten ticks, every contender
// run `runs` times taking turns, each block counted at its fastest.
import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const argument = (name, fallback) => { const at = process.argv.indexOf(name); return at < 0 ? fallback : process.argv[at + 1]; };
const runs = Number(argument('--runs', 3));
const sizes = argument('--sizes', '100,1000,10000').split(',').map(Number);
const esDirectory = argument('--es', null);
const blocks = 30, ticksPerBlock = 10, ticks = blocks * ticksPerBlock;

const load = (directory) => async (module) => import(pathToFileURL(join(directory, module, 'index.js')));
const stock = load(join(here, '..', 'output'));
const Setup = await stock('Hylograph.ForceEngine.Setup');
const PureSim = await stock('Hylograph.ForceEngine.Simulation');
const WasmSetup = await stock('Hylograph.ForceEngine.Setup.WASM');
const es = esDirectory ? load(resolve(esDirectory)) : null;
const EsSetup = es && await es('Hylograph.ForceEngine.Setup');
const EsSim = es && await es('Hylograph.ForceEngine.Simulation');

// The library loads the kernel with fetch(); give Node a fetch that reads files.
const realFetch = globalThis.fetch;
globalThis.fetch = async (url, ...rest) => {
  const href = String(url instanceof Request ? url.url : url);
  if (!href.startsWith('file:')) return realFetch(url, ...rest);
  return new Response(readFileSync(fileURLToPath(href)), { headers: { 'content-type': 'application/wasm' } });
};
const kernelUrl = pathToFileURL(join(homedir(), 'work/afc-work/purescript-hylograph-showcases/wasm-force-demo/pkg/force_kernel.js')).href;
const { initWasm_ } = await import(pathToFileURL(join(here, '..', 'output', 'Hylograph.ForceEngine.WASM/foreign.js')));
const log = console.log; console.log = () => {};
await new Promise((done, failed) => initWasm_(kernelUrl)(failed, done));
console.log = log;

const spiral = (i) => { const r = 10 * Math.sqrt(0.5 + i), a = i * Math.PI * (3 - Math.sqrt(5)); return [r * Math.cos(a), r * Math.sin(a)]; };
const makeNodes = (n) => Array.from({ length: n }, (_, id) => { const [x, y] = spiral(id); return { id, x, y, vx: 0, vy: 0, fx: null, fy: null }; });
const makeLinks = (n) => {
  const links = [];
  for (let i = 0; i < n; i++) { links.push([i, (i + 1) % n]); if (i % 7 === 0) links.push([i, (i * 13 + 5) % n]); }
  return links.filter(([a, b]) => a !== b).map(([source, target]) => ({ source, target }));
};
const params = { alpha: 1, alphaMin: 0.001, alphaDecay: 1 - Math.pow(0.001, 1 / 300), alphaTarget: 0, velocityRetention: 0.6 };
const setupWith = (S) => S.setupWithParams('timing')([
  S.withTheta(0.9)(S.withStrength(S.static(-30))(S.manyBody('charge'))),
  S.withDegreeStrength(S.withDistance(S.static(30))(S.link('links'))),
  S.center('middle'),
])(params);

const viaPure = (S, Sim) => (n) => {
  const sim = Sim.create({ alphaMin: params.alphaMin, alphaDecay: params.alphaDecay, alphaTarget: params.alphaTarget, velocityRetention: params.velocityRetention })();
  Sim.setNodes(makeNodes(n))(sim)();
  Sim.setLinks(makeLinks(n))(sim)();
  S.applySetup(setupWith(S))(sim)();
  const tick = Sim.tick(sim);
  return { tick, done: () => {}, nodes: () => sim.nodes.value };
};
const viaWasm = (n) => {
  const sim = WasmSetup.create(makeNodes(n))(makeLinks(n))();
  WasmSetup.applySetup(setupWith(Setup))(sim)();
  const tick = WasmSetup.tick(sim);
  return { tick, done: () => WasmSetup.free(sim)(), nodes: () => WasmSetup.getNodes(sim)() };
};

const contenders = [['default engine', viaPure(Setup, PureSim)], ['WASM engine', viaWasm]];
if (es) contenders.splice(1, 0, ['default engine, purs-backend-es', viaPure(EsSetup, EsSim)]);

console.log(`many-body + link + centre through the library's loop, ${ticks} ticks, ${runs} runs each taking turns; ms per tick, each block of ${ticksPerBlock} ticks at its fastest`);
for (const n of sizes) {
  const fastest = contenders.map(() => new Array(blocks).fill(Infinity));
  let moved = true;
  for (let run = 0; run < runs; run++) contenders.forEach(([, make], c) => {
    const { tick, done, nodes } = make(n);
    for (let block = 0; block < blocks; block++) {
      const start = performance.now();
      for (let i = 0; i < ticksPerBlock; i++) tick();
      fastest[c][block] = Math.min(fastest[c][block], performance.now() - start);
    }
    const [x0] = spiral(0);
    moved = moved && nodes().every((d) => Number.isFinite(d.x) && Number.isFinite(d.y)) && Math.abs(nodes()[0].x - x0) > 1e-6;
    done();
  });
  const perTick = fastest.map((times) => times.reduce((a, b) => a + b, 0) / ticks);
  console.log(`${String(n).padStart(6)} nodes:  ` + contenders.map(([name], c) => `${name} ${perTick[c].toFixed(3)}`).join('   ') + `   (default / WASM ${(perTick[0] / perTick[perTick.length - 1]).toFixed(1)}x)${moved ? '' : '   NODES DID NOT MOVE'}`);
}
