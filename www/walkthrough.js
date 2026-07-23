/* Nonconformity walkthrough.
   Seven slides, each with an original inline SVG vignette on a fixed
   stage. The tour opens on the first visit, and any time from the header
   button. Every vignette uses the live theme tokens so the pictures match
   whatever palette and mode the person picked. */

(function () {
  var SEEN_KEY = 'nonconformity-tour-seen';
  var stageW = 300, stageH = 160;

  function stage(inner) {
    return '<svg width="' + stageW + '" height="' + stageH + '" viewBox="0 0 ' +
      stageW + ' ' + stageH + '" role="img" aria-hidden="true">' + inner + '</svg>';
  }

  /* A wobbling line with one tall spike, the app in one picture. */
  function vWelcome() {
    return stage(
      '<path d="M 10 110 L 50 100 L 90 108 L 120 40 L 150 108 L 200 98 L 250 104 L 290 96"' +
      ' fill="none" stroke="var(--accent)" stroke-width="3" stroke-linecap="round"/>' +
      '<path d="M 120 22 L 129 40 L 111 40 Z" fill="var(--spike)"/>' +
      '<text x="150" y="145" text-anchor="middle" font-size="13" fill="var(--muted)">One reading stands apart</text>'
    );
  }

  /* A tiny CSV sheet with two columns. */
  function vData() {
    var rows = '';
    for (var i = 0; i < 4; i++) {
      var y = 46 + i * 24;
      rows += '<rect x="70" y="' + y + '" width="76" height="16" rx="3" fill="var(--surface2)" stroke="var(--border)"/>' +
              '<rect x="156" y="' + y + '" width="60" height="16" rx="3" fill="var(--surface2)" stroke="var(--border)"/>';
    }
    return stage(
      '<rect x="58" y="16" width="170" height="130" rx="10" fill="var(--surface)" stroke="var(--border)" stroke-width="2"/>' +
      '<text x="108" y="38" text-anchor="middle" font-size="12" fill="var(--text)">stamp</text>' +
      '<text x="186" y="38" text-anchor="middle" font-size="12" fill="var(--text)">value</text>' + rows +
      '<text x="250" y="90" font-size="12" fill="var(--muted)">.csv</text>'
    );
  }

  /* Trend plus season: a slow rise with a repeating wave on top. */
  function vPattern() {
    return stage(
      '<path d="M 15 120 L 285 78" fill="none" stroke="var(--muted)" stroke-width="2" stroke-dasharray="6 5"/>' +
      '<path d="M 15 118 Q 35 92 55 114 Q 75 134 95 108 Q 115 84 135 106 Q 155 126 175 100 Q 195 76 215 98 Q 235 118 255 92 Q 270 76 285 84"' +
      ' fill="none" stroke="var(--accent)" stroke-width="3"/>' +
      '<text x="150" y="150" text-anchor="middle" font-size="13" fill="var(--muted)">A rhythm rides a slow trend</text>'
    );
  }

  /* The expected band with one point outside it. */
  function vBand() {
    return stage(
      '<path d="M 15 60 L 285 52 L 285 108 L 15 116 Z" fill="var(--band)"/>' +
      '<path d="M 15 88 L 80 84 L 140 86 L 180 30 L 220 84 L 285 80" fill="none" stroke="var(--accent)" stroke-width="3"/>' +
      '<path d="M 180 14 L 189 32 L 171 32 Z" fill="var(--spike)"/>' +
      '<text x="150" y="148" text-anchor="middle" font-size="13" fill="var(--muted)">Outside the band means a card</text>'
    );
  }

  /* The four marker shapes with their names. */
  function vShapes() {
    return stage(
      '<path d="M 45 44 L 57 68 L 33 68 Z" fill="var(--spike)"/>' +
      '<text x="45" y="88" text-anchor="middle" font-size="12" fill="var(--text)">Spike</text>' +
      '<path d="M 111 44 L 135 44 L 123 68 Z" fill="var(--dip)"/>' +
      '<text x="123" y="88" text-anchor="middle" font-size="12" fill="var(--text)">Dip</text>' +
      '<rect x="181" y="50" width="30" height="13" rx="3" fill="var(--run)"/>' +
      '<text x="196" y="88" text-anchor="middle" font-size="12" fill="var(--text)">Run</text>' +
      '<path d="M 262 42 L 276 56 L 262 70 L 248 56 Z" fill="var(--shift)"/>' +
      '<text x="262" y="88" text-anchor="middle" font-size="12" fill="var(--text)">Shift</text>' +
      '<text x="150" y="128" text-anchor="middle" font-size="13" fill="var(--muted)">Shape carries meaning, not color alone</text>'
    );
  }

  /* A reading card sketch with chips. */
  function vCards() {
    return stage(
      '<rect x="40" y="24" width="220" height="112" rx="12" fill="var(--surface)" stroke="var(--border)" stroke-width="2"/>' +
      '<rect x="40" y="24" width="6" height="112" rx="3" fill="var(--spike)"/>' +
      '<path d="M 62 40 L 72 58 L 52 58 Z" fill="var(--spike)"/>' +
      '<rect x="82" y="42" width="110" height="10" rx="5" fill="var(--surface2)"/>' +
      '<rect x="58" y="70" width="186" height="7" rx="3" fill="var(--surface2)"/>' +
      '<rect x="58" y="84" width="160" height="7" rx="3" fill="var(--surface2)"/>' +
      '<rect x="58" y="104" width="58" height="16" rx="8" fill="var(--surface2)" stroke="var(--border)"/>' +
      '<rect x="124" y="104" width="58" height="16" rx="8" fill="var(--surface2)" stroke="var(--border)"/>' +
      '<text x="150" y="152" text-anchor="middle" font-size="13" fill="var(--muted)">Plain words plus the key numbers</text>'
    );
  }

  /* A plug meeting a socket for the optional local model. */
  function vModel() {
    return stage(
      '<rect x="52" y="58" width="76" height="44" rx="10" fill="var(--surface)" stroke="var(--accent)" stroke-width="2.5"/>' +
      '<line x1="128" y1="70" x2="150" y2="70" stroke="var(--accent)" stroke-width="4" stroke-linecap="round"/>' +
      '<line x1="128" y1="90" x2="150" y2="90" stroke="var(--accent)" stroke-width="4" stroke-linecap="round"/>' +
      '<rect x="160" y="50" width="90" height="60" rx="10" fill="var(--surface2)" stroke="var(--border)" stroke-width="2"/>' +
      '<circle cx="188" cy="70" r="4" fill="var(--muted)"/>' +
      '<circle cx="188" cy="90" r="4" fill="var(--muted)"/>' +
      '<text x="222" y="85" font-size="12" fill="var(--text)">local</text>' +
      '<text x="150" y="142" text-anchor="middle" font-size="13" fill="var(--muted)">Optional, on your machine only</text>'
    );
  }

  var slides = [
    { title: 'Welcome to Nonconformity',
      body: 'Nonconformity reads a series of timestamped numbers, learns its ordinary rhythm, and points at the moments that break it. Classical statistics do the work, so every flag can be explained.',
      art: vWelcome },
    { title: 'Bring your data',
      body: 'A plain CSV with a timestamp column and a value column is all it takes. Drop it on the left, or open a sample to see the whole app working in one click. Everything stays on your computer.',
      art: vData },
    { title: 'The expected pattern',
      body: 'A running median follows the slow trend, and per position medians pick up any repeating cycle, like mornings versus nights or weekdays versus weekends. Together they form the expected level at every moment.',
      art: vPattern },
    { title: 'How flagging works',
      body: 'Around the expected level sits a band whose width is a multiple of the usual wobble. The sensitivity slider sets that multiple. Readings outside the band become events, and lasting level changes are found separately.',
      art: vBand },
    { title: 'Four kinds of events',
      body: 'A spike, a dip, a sustained run, and a level shift each get their own marker shape. Shapes carry the meaning alongside color, so the chart still reads under any color vision and in gray print.',
      art: vShapes },
    { title: 'The reading cards',
      body: 'Every event gets one card with plain language, the observed and expected values, and its size in multiples of the usual wobble. Cards and markers link both ways.',
      art: vCards },
    { title: 'An optional local model',
      body: 'A language model running on your own machine can summarize the whole run, add a cause note to each card, answer typed questions about the data, and draft an incident writeup. It reads the analysis, never your raw readings, and the numbers never depend on it.',
      art: vModel },
    { title: 'Two ways to look',
      body: 'The Operator view keeps things plain: chart, readings, a few honest controls. The Researcher view adds the method internals, the raw event scores, and the exact call to reproduce a run. Switch any time from the header.',
      art: vPersona }
  ];

  /* Two panes, one plain and one with extra detail lines, for the persona
     switch. */
  function vPersona() {
    return stage(
      '<rect x="26" y="34" width="110" height="92" rx="10" fill="var(--surface)" stroke="var(--border)" stroke-width="2"/>' +
      '<path d="M 38 96 L 60 88 L 78 66 L 98 92 L 124 82" fill="none" stroke="var(--accent)" stroke-width="2.5"/>' +
      '<text x="81" y="118" text-anchor="middle" font-size="11" fill="var(--muted)">Operator</text>' +
      '<rect x="164" y="34" width="110" height="92" rx="10" fill="var(--surface)" stroke="var(--accent)" stroke-width="2"/>' +
      '<path d="M 176 96 L 198 88 L 216 66 L 236 92 L 262 82" fill="none" stroke="var(--accent)" stroke-width="2.5"/>' +
      '<line x1="176" y1="106" x2="262" y2="106" stroke="var(--muted)" stroke-width="1"/>' +
      '<line x1="176" y1="113" x2="248" y2="113" stroke="var(--muted)" stroke-width="1"/>' +
      '<text x="219" y="118" text-anchor="middle" font-size="11" fill="var(--muted)">Researcher</text>'
    );
  }

  var idx = 0;
  var veil = null;

  function seen() {
    try { return localStorage.getItem(SEEN_KEY) === 'yes'; } catch (e) { return true; }
  }
  function markSeen() {
    try { localStorage.setItem(SEEN_KEY, 'yes'); } catch (e) {}
  }

  /* Each paint rebuilds the card from the slide table, so the arrows,
     dots, and vignette can never drift out of step with each other. */
  function paint() {
    if (!veil) return;
    var s = slides[idx];
    var dots = slides.map(function (_, i) {
      return '<span class="tour-dot' + (i === idx ? ' on' : '') + '"></span>';
    }).join('');
    veil.innerHTML =
      '<div class="tour-card" role="dialog" aria-modal="true" aria-label="Walkthrough">' +
      '<h2 class="tour-title">' + s.title + '</h2>' +
      '<div class="tour-stage">' + s.art() + '</div>' +
      '<p class="tour-body">' + s.body + '</p>' +
      '<div class="tour-nav">' +
      '<button type="button" class="btn-ghost" id="tour-back"' + (idx === 0 ? ' disabled' : '') + '>Back</button>' +
      '<button type="button" class="btn-solid" id="tour-next">' + (idx === slides.length - 1 ? 'Start' : 'Next') + '</button>' +
      '<button type="button" class="btn-ghost" id="tour-skip">Skip</button>' +
      '<div class="tour-dots">' + dots + '</div>' +
      '</div></div>';
    var next = veil.querySelector('#tour-next');
    if (next) next.focus();
  }

  /* Opening is idempotent: a second call while the veil is up does
     nothing, which makes the header button safe to mash. */
  function open() {
    if (veil) return;
    idx = 0;
    veil = document.createElement('div');
    veil.className = 'tour-veil';
    document.body.appendChild(veil);
    paint();
  }

  /* Closing marks the tour as seen and hands focus back to the opener,
     so keyboard users land where they left. */
  function close() {
    if (!veil) return;
    veil.remove();
    veil = null;
    markSeen();
    var opener = document.getElementById('tour-open');
    if (opener) opener.focus();
  }

  document.addEventListener('click', function (e) {
    var t = e.target;
    if (!t) return;
    if (t.id === 'tour-open') { open(); return; }
    if (!veil) return;
    if (t.id === 'tour-next') {
      if (idx === slides.length - 1) close();
      else { idx += 1; paint(); }
    }
    if (t.id === 'tour-back' && idx > 0) { idx -= 1; paint(); }
    if (t.id === 'tour-skip') close();
  });

  document.addEventListener('keydown', function (e) {
    if (!veil) return;
    if (e.key === 'Escape') close();
    if (e.key === 'ArrowRight' && idx < slides.length - 1) { idx += 1; paint(); }
    if (e.key === 'ArrowLeft' && idx > 0) { idx -= 1; paint(); }
  });

  /* The server can also open the tour through a custom message, and the
     registration retries until Shiny exists rather than racing it. */
  function register() {
    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler('open-walkthrough', function () { open(); });
    } else {
      setTimeout(register, 120);
    }
  }
  register();

  function boot() {
    if (!seen()) open();
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }

  /* Exposed for the DOM test harness. */
  window.NcTour = { open: open, close: close, slides: slides };
})();
