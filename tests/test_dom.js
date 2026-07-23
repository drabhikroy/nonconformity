/* DOM level tests for Nonconformity.
   jsdom stands in for the browser so rendering failures show up headlessly
   before any human review. The checks cover the column wise row
   conversion, the marker geometry, keyboard focus order, the tour, and
   the palette state machine. */

const fs = require('fs');
const { JSDOM } = require('jsdom');

const dom = new JSDOM('<!doctype html><html><body>' +
  '<div class="chart-wrap"><div id="chart-host"></div>' +
  '<div id="chart-tip"></div><div id="chart-live"></div></div>' +
  '<button id="theme-toggle"></button>' +
  '<button class="seg-btn persona-btn" data-persona="operator" aria-pressed="true"></button>' +
  '<button class="seg-btn persona-btn" data-persona="researcher" aria-pressed="false"></button>' +
  '<button class="seg-btn" data-palette="standard"></button>' +
  '<button class="seg-btn" data-palette="cb-mono"></button>' +
  '<input type="checkbox" id="show-band" checked>' +
  '<input type="checkbox" id="show-trend" checked>' +
  '<input type="checkbox" id="show-marks" checked>' +
  '<button id="tour-open"></button>' +
  '<div class="researcher-only" id="researcher-panel"></div>' +
  '<div id="model-actions" style="display:none;">' +
  '<button id="model-triage"></button><button id="model-notes"></button>' +
  '<button id="model-incident"></button></div>' +
  '<div id="triage-card" style="display:none;"><h2 id="triage-title"></h2><div id="triage-body"></div></div>' +
  '<div id="chat-card" style="display:none;"><div id="chat-log"></div>' +
  '<input id="chat-input"><button id="chat-send"></button></div>' +
  '</body></html>', { pretendToBeVisual: true });
const window = dom.window;
global.window = window;
global.document = window.document;

let ok = 0, bad = 0;
function check(label, cond) {
  if (cond) { ok += 1; console.log('pass  ' + label); }
  else { bad += 1; console.log('FAIL  ' + label); }
}

/* A Shiny stub present before the scripts load, so every retrying
   registration resolves on its first try. */
const handlers = {};
window.Shiny = {
  addCustomMessageHandler: (name, fn) => { handlers[name] = fn; },
  setInputValue: () => {},
};
window.matchMedia = () => ({ matches: false });
global.requestAnimationFrame = (fn) => fn();

for (const f of ['www/ui.js', 'www/chart.js', 'www/walkthrough.js', 'www/model.js']) {
  window.eval(fs.readFileSync(f, 'utf8'));
}

check('chart handler registered', typeof handlers['render-chart'] === 'function');
check('tour handler registered', typeof handlers['open-walkthrough'] === 'function');
check('NcChart exposed', typeof window.NcChart === 'object');
check('NcTour exposed', typeof window.NcTour === 'object');
check('NcUi exposed', typeof window.NcUi === 'object');
check('NcModel exposed', typeof window.NcModel === 'object');
check('model exposes triage, notes, incident, and chat',
  typeof window.NcModel.triage === 'function' &&
  typeof window.NcModel.annotate === 'function' &&
  typeof window.NcModel.incident === 'function' &&
  typeof window.NcModel.ask === 'function');

/* Row conversion must undo the column wise serialization. */
const rows = window.NcChart.toRows({ a: [1, 2, 3], b: ['x', 'y', 'z'] });
check('toRows length', rows.length === 3);
check('toRows fields', rows[1].a === 2 && rows[1].b === 'y');
check('toRows passes arrays through',
  window.NcChart.toRows([{ a: 1 }]).length === 1);

/* Marker geometry stays at its anchor rather than collapsing to origin. */
const d = window.NcChart.markerPath('spike', 200, 100, 9);
check('marker path anchored at its point', d.indexOf('200') >= 0 && d.indexOf('91') >= 0);

/* A tiny payload in column wise shape renders a full chart. */
const n = 60;
const stamps = [], values = [], trend = [], expected = [], lo = [], hi = [];
for (let i = 0; i < n; i++) {
  stamps.push(new Date(Date.UTC(2026, 5, 1, i)).toISOString());
  const v = 100 + 10 * Math.sin(i / 4) + (i === 30 ? 80 : 0);
  values.push(v); trend.push(100); expected.push(100 + 10 * Math.sin(i / 4));
  lo.push(expected[i] - 25); hi.push(expected[i] + 25);
}
handlers['render-chart']({
  series: { stamp: stamps, value: values, trend: trend, expected: expected, lo: lo, hi: hi },
  events: { id: ['ev1'], type: ['spike'], start: [31], end: [31], peak: [31],
            peak_score: [3.2], mean_score: [3.2], len: [1], direction: ['above'],
            weight: [11.2], title: ['Spike'], when: ['Jun 02, 06:00'] },
  meta: { cadence: 'hourly', source: 'test' }
});
const svg = document.querySelector('#chart-host svg');
check('svg rendered', !!svg);
check('value line present', svg && svg.querySelectorAll('path').length >= 2);
const marker = svg && svg.querySelector('[data-event="ev1"]');
check('event marker present and focusable',
  !!marker && marker.getAttribute('tabindex') === '0');
check('marker carries an aria label',
  !!marker && (marker.getAttribute('aria-label') || '').indexOf('Spike') === 0);
check('axis labels painted', svg && svg.querySelectorAll('text').length >= 8);

/* Toggling markers off removes them on the next render. */
document.getElementById('show-marks').checked = false;
window.NcChart.render();
check('marker toggle removes markers',
  !document.querySelector('#chart-host [data-event="ev1"]'));
document.getElementById('show-marks').checked = true;
window.NcChart.render();

/* The tour opens, pages, and closes. */
window.NcTour.open();
check('tour veil opens', !!document.querySelector('.tour-veil'));
check('tour has eight slides', window.NcTour.slides.length === 8);
check('slide vignette is an svg',
  document.querySelector('.tour-stage svg') !== null);
window.NcTour.close();
check('tour closes', !document.querySelector('.tour-veil'));

/* Palette state machine sets classes and pressed states. */
window.NcUi.applyPalette('cb-mono');
check('mono class applied', document.body.classList.contains('cb-mono'));
const monoBtn = document.querySelector('[data-palette="cb-mono"]');
check('mono button pressed', monoBtn.getAttribute('aria-pressed') === 'true');
window.NcUi.applyPalette('standard');
check('standard clears palette classes', !document.body.classList.contains('cb-mono'));

/* Persona switch. The stylesheet does the revealing, so the body class and
   the pressed state are what there is to assert on here; jsdom applies no
   stylesheet, which is exactly why the inline style is gone. */
window.NcUi.applyPersona('researcher');
check('researcher body class set', document.body.classList.contains('show-researcher'));
const rBtn = document.querySelector('[data-persona="researcher"]');
check('researcher button pressed', rBtn.getAttribute('aria-pressed') === 'true');
check('operator button released',
  document.querySelector('[data-persona="operator"]').getAttribute('aria-pressed') === 'false');
check('researcher panel carries the class the stylesheet targets',
  document.getElementById('researcher-panel').classList.contains('researcher-only'));
window.NcUi.applyPersona('operator');
check('operator clears researcher class', !document.body.classList.contains('show-researcher'));
check('operator button pressed after switching back',
  document.querySelector('[data-persona="operator"]').getAttribute('aria-pressed') === 'true');

/* The model layer against a stubbed endpoint. These cover the split between
   a model that answers with nothing and a machine that never answered,
   which are different problems and used to produce the same message. */
const samplePayload = {
  series: { stamp: stamps, value: values, trend: trend, expected: expected, lo: lo, hi: hi },
  events: { id: ['ev1'], type: ['spike'], start: [31], end: [31], peak: [31],
            peak_score: [3.2], mean_score: [3.2], len: [1], direction: ['above'],
            weight: [11.2], title: ['Spike'], when: ['Jun 02, 06:00'] },
  meta: { cadence: 'hourly', source: 'test', method: 'robust',
          method_label: 'Resistant seasonal', sens: 4.5, n: 60, summary: 'A test run.' }
};

/* The modules are evaluated inside the jsdom window, but a bare fetch call
   still resolves against the Node global, so both have to be stubbed or the
   test quietly exercises a real connection attempt instead. */
function stubFetch(mode) {
  const impl = function () {
    if (mode === 'reject') return Promise.reject(new Error('down'));
    if (mode === 'httperr') return Promise.resolve({ ok: false, status: 500 });
    return Promise.resolve({
      ok: true,
      json: function () { return Promise.resolve({ response: 'Looks routine overall.' }); }
    });
  };
  window.fetch = impl;
  global.fetch = impl;
}

/* Two promise hops plus a DOM write, so give the queue a real tick rather
   than guessing at microtask counts. */
const settle = () => new Promise(function (r) { setTimeout(r, 10); });

(async function () {
  window.NcModel.setPayload(samplePayload);

  stubFetch('ok');
  window.NcModel.triage();
  await settle();
  check('triage writes its heading',
    document.getElementById('triage-title').textContent === 'Model summary of this run');
  check('triage shows the model text',
    document.getElementById('triage-body').textContent.indexOf('Looks routine') >= 0);

  window.NcModel.incident();
  await settle();
  check('incident retitles the shared card',
    document.getElementById('triage-title').textContent === 'Draft incident writeup');

  stubFetch('reject');
  window.NcModel.triage();
  await settle();
  check('unreachable model reports a connection problem',
    document.getElementById('triage-body').textContent.indexOf('could not be reached') >= 0);

  stubFetch('httperr');
  window.NcModel.triage();
  await settle();
  check('an http error is treated as unreachable rather than an empty answer',
    document.getElementById('triage-body').textContent.indexOf('could not be reached') >= 0);

  console.log('\n' + ok + ' passed, ' + bad + ' failed');
  process.exit(bad > 0 ? 1 : 0);
})();
