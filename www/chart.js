/* Nonconformity chart renderer.
   One hand built SVG, no chart library, so every mark is inspectable and
   the accessibility layer is first class rather than bolted on. All
   element positions use absolute coordinates on the elements themselves,
   never a group offset, which keeps the geometry easy to test. */

(function () {
  var NS = 'http://www.w3.org/2000/svg';
  var W = 960, H = 420;
  var PAD = { top: 18, right: 22, bottom: 44, left: 64 };
  var state = { payload: null, focus: 'all' };

  /* Shiny serializes data frames column wise over the socket. This turns
     an object of parallel arrays back into an array of row records, and
     passes real arrays straight through. */
  function toRows(frame) {
    if (!frame) return [];
    if (Array.isArray(frame)) return frame;
    var keys = Object.keys(frame);
    if (keys.length === 0) return [];
    var first = frame[keys[0]];
    var n = Array.isArray(first) ? first.length : 1;
    var rows = [];
    for (var i = 0; i < n; i++) {
      var row = {};
      keys.forEach(function (k) {
        row[k] = Array.isArray(frame[k]) ? frame[k][i] : frame[k];
      });
      rows.push(row);
    }
    return rows;
  }

  /* A tiny element helper keeps the render body readable; attribute
     maps double as documentation of what each mark carries. */
  function el(name, attrs) {
    var node = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) {
      node.setAttribute(k, attrs[k]);
    });
    return node;
  }

  function reducedMotion() {
    return window.matchMedia &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  }

  /* Axis numbers stay short: thousands get separators, small values keep
     two decimals, because dense tick labels fight the data for eyes. */
  function fmtVal(v) {
    if (Math.abs(v) >= 1000) return Math.round(v).toLocaleString();
    return (Math.round(v * 100) / 100).toString();
  }

  /* Tick wording follows the visible span: years across years, days
     across weeks, clock times inside a day. */
  function fmtTick(d, spanDays) {
    if (spanDays > 400) return d.toLocaleDateString(undefined, { month: 'short', year: 'numeric' });
    if (spanDays > 3) return d.toLocaleDateString(undefined, { month: 'short', day: 'numeric' });
    return d.toLocaleTimeString(undefined, { hour: '2-digit', minute: '2-digit' });
  }

  /* Marker path builders. Each returns a d string at absolute coordinates
     because a path built at the origin and shifted by a group offset is
     exactly the geometry mistake that once collapsed shapes to a corner. */
  function markerPath(type, cx, cy, r) {
    if (type === 'spike') {
      return 'M ' + cx + ' ' + (cy - r) +
             ' L ' + (cx + r) + ' ' + (cy + r) +
             ' L ' + (cx - r) + ' ' + (cy + r) + ' Z';
    }
    if (type === 'dip') {
      return 'M ' + (cx - r) + ' ' + (cy - r) +
             ' L ' + (cx + r) + ' ' + (cy - r) +
             ' L ' + cx + ' ' + (cy + r) + ' Z';
    }
    if (type === 'shift') {
      return 'M ' + cx + ' ' + (cy - r) +
             ' L ' + (cx + r) + ' ' + cy +
             ' L ' + cx + ' ' + (cy + r) +
             ' L ' + (cx - r) + ' ' + cy + ' Z';
    }
    return 'M ' + (cx - r) + ' ' + (cy - r * 0.55) +
           ' L ' + (cx + r) + ' ' + (cy - r * 0.55) +
           ' L ' + (cx + r) + ' ' + (cy + r * 0.55) +
           ' L ' + (cx - r) + ' ' + (cy + r * 0.55) + ' Z';
  }

  /* The focus control trades context for detail without any zoom UI:
     whole series, a window around the heaviest event, or the tail. */
  function windowRows(rows, events) {
    if (state.focus === 'all' || rows.length === 0) return rows;
    if (state.focus === 'tail') {
      return rows.slice(Math.floor(rows.length * 0.75));
    }
    if (state.focus === 'biggest' && events.length > 0) {
      var big = events.reduce(function (a, b) {
        return (b.weight > a.weight) ? b : a;
      });
      var half = Math.max(30, Math.floor(rows.length * 0.08));
      var lo = Math.max(0, big.peak - 1 - half);
      var hi = Math.min(rows.length, big.peak - 1 + half);
      return rows.slice(lo, hi);
    }
    return rows;
  }

  /* The live region speaks for keyboard and screen reader users; text
     assignment is enough because the region is marked polite. */
  function announce(text) {
    var live = document.getElementById('chart-live');
    if (live) live.textContent = text;
  }

  /* The tooltip is positioned inside the wrap so it never escapes the
     card, and it follows focus as well as the pointer. */
  function showTip(html, x, y, wrap) {
    var tip = document.getElementById('chart-tip');
    if (!tip) return;
    tip.innerHTML = html;
    tip.style.display = 'block';
    var box = wrap.getBoundingClientRect();
    var left = Math.min(x + 14, box.width - 270);
    tip.style.left = Math.max(0, left) + 'px';
    tip.style.top = Math.max(0, y - 10) + 'px';
  }

  function hideTip() {
    var tip = document.getElementById('chart-tip');
    if (tip) tip.style.display = 'none';
  }

  /* A thrown error inside a redraw would otherwise leave an empty card and
     no clue why. Reporting it where the chart belongs keeps the rest of the
     page usable and gives the person something to send on. */
  function render() {
    try {
      drawChart();
    } catch (err) {
      var host = document.getElementById('chart-host');
      if (host) {
        host.innerHTML = '<p class="hint">The chart could not be rendered: ' +
          String(err && err.message ? err.message : err) +
          '. The readings below are unaffected.</p>';
      }
      if (window.console) console.error('Nonconformity chart render failed', err);
    }
  }

  function drawChart() {
    var host = document.getElementById('chart-host');
    if (!host || !state.payload) return;
    var rows = toRows(state.payload.series);
    var events = toRows(state.payload.events);
    rows.forEach(function (r) { r.t = new Date(r.stamp).getTime(); });
    var all = rows;
    var view = windowRows(rows, events);

    host.innerHTML = '';
    var svg = el('svg', {
      viewBox: '0 0 ' + W + ' ' + H,
      role: 'img',
      'aria-label': 'Time series with flagged events'
    });

    if (view.length === 0) { host.appendChild(svg); return; }

    var t0 = view[0].t, t1 = view[view.length - 1].t;
    var vals = [];
    view.forEach(function (r) { vals.push(r.value, r.lo, r.hi); });
    var y0 = Math.min.apply(null, vals), y1 = Math.max.apply(null, vals);
    var ypad = (y1 - y0) * 0.06 || 1;
    y0 -= ypad; y1 += ypad;

    function X(t) { return PAD.left + (t - t0) / (t1 - t0 || 1) * (W - PAD.left - PAD.right); }
    function Y(v) { return PAD.top + (y1 - v) / (y1 - y0 || 1) * (H - PAD.top - PAD.bottom); }

    var showBand = isOn('show-band');
    var showTrend = isOn('show-trend');
    var showMarks = isOn('show-marks');

    /* Grid and axes. */
    var yTicks = 5;
    for (var g = 0; g <= yTicks; g++) {
      var gv = y0 + (y1 - y0) * g / yTicks;
      var gy = Y(gv);
      svg.appendChild(el('line', {
        x1: PAD.left, x2: W - PAD.right, y1: gy, y2: gy,
        stroke: 'var(--border)', 'stroke-width': 1, opacity: 0.55
      }));
      var lab = el('text', {
        x: PAD.left - 10, y: gy + 4, 'text-anchor': 'end',
        'font-size': 12, fill: 'var(--muted)'
      });
      lab.textContent = fmtVal(gv);
      svg.appendChild(lab);
    }
    var spanDays = (t1 - t0) / 86400000;
    var xTicks = 6;
    for (var x = 0; x <= xTicks; x++) {
      var tv = t0 + (t1 - t0) * x / xTicks;
      var tx = X(tv);
      var xlab = el('text', {
        x: tx, y: H - PAD.bottom + 22, 'text-anchor': 'middle',
        'font-size': 12, fill: 'var(--muted)'
      });
      xlab.textContent = fmtTick(new Date(tv), spanDays);
      svg.appendChild(xlab);
    }

    /* Expected range band, painted first so everything sits above it. */
    if (showBand) {
      var top = view.map(function (r) { return X(r.t) + ' ' + Y(r.hi); });
      var bottom = view.slice().reverse().map(function (r) { return X(r.t) + ' ' + Y(r.lo); });
      svg.appendChild(el('path', {
        d: 'M ' + top.join(' L ') + ' L ' + bottom.join(' L ') + ' Z',
        fill: 'var(--band)', stroke: 'none'
      }));
    }

    /* Sustained runs get a translucent column so the whole stretch reads
       as one event, not a pile of separate dots. */
    if (showMarks) {
      events.forEach(function (ev) {
        if (ev.type !== 'run') return;
        var s = all[ev.start - 1], e = all[ev.end - 1];
        if (!s || !e || e.t < t0 || s.t > t1) return;
        svg.appendChild(el('rect', {
          x: X(Math.max(s.t, t0)), y: PAD.top,
          width: Math.max(2, X(Math.min(e.t, t1)) - X(Math.max(s.t, t0))),
          height: H - PAD.top - PAD.bottom,
          fill: 'var(--run)', opacity: 0.12
        }));
      });
    }

    if (showTrend) {
      var tpath = view.map(function (r, i) {
        return (i === 0 ? 'M ' : 'L ') + X(r.t) + ' ' + Y(r.trend);
      }).join(' ');
      svg.appendChild(el('path', {
        d: tpath, fill: 'none', stroke: 'var(--muted)',
        'stroke-width': 1.6, 'stroke-dasharray': '6 5'
      }));
    }

    /* The value line itself, with a draw in entrance unless the person
       asked the system for reduced motion. */
    var vpath = view.map(function (r, i) {
      return (i === 0 ? 'M ' : 'L ') + X(r.t) + ' ' + Y(r.value);
    }).join(' ');
    var line = el('path', {
      d: vpath, fill: 'none', stroke: 'var(--accent)',
      'stroke-width': 2, 'stroke-linejoin': 'round'
    });
    svg.appendChild(line);
    if (!reducedMotion()) {
      var len = 3200;
      line.style.strokeDasharray = len;
      line.style.strokeDashoffset = len;
      line.style.transition = 'stroke-dashoffset 900ms ease';
      requestAnimationFrame(function () {
        requestAnimationFrame(function () { line.style.strokeDashoffset = 0; });
      });
    }

    /* Shift lines: a vertical dashed rule at the cut point. */
    if (showMarks) {
      events.forEach(function (ev) {
        if (ev.type !== 'shift') return;
        var r = all[ev.peak - 1];
        if (!r || r.t < t0 || r.t > t1) return;
        svg.appendChild(el('line', {
          x1: X(r.t), x2: X(r.t), y1: PAD.top, y2: H - PAD.bottom,
          stroke: 'var(--shift)', 'stroke-width': 2, 'stroke-dasharray': '4 5'
        }));
      });
    }

    /* One focusable marker per event, in document order, so Tab and the
       arrow keys walk the story left to right. */
    if (showMarks) {
      var focusables = [];
      events.forEach(function (ev) {
        var r = all[ev.peak - 1];
        if (!r || r.t < t0 || r.t > t1) return;
        var cx = X(r.t), cy = Y(r.value);
        if (ev.type === 'shift') cy = PAD.top + 16;
        var mark = el('path', {
          d: markerPath(ev.type, cx, cy, 9),
          fill: 'var(--' + ev.type + ')',
          stroke: 'var(--surface)', 'stroke-width': 1.5,
          tabindex: 0, role: 'button',
          'data-event': ev.id,
          'aria-label': ev.title + ', ' + ev.when + '. Press Enter to open its card.'
        });
        if (!reducedMotion()) {
          mark.style.opacity = 0;
          mark.style.transition = 'opacity 400ms ease 700ms';
          requestAnimationFrame(function () {
            requestAnimationFrame(function () { mark.style.opacity = 1; });
          });
        }
        var tipHtml = '<strong>' + ev.title + '</strong><br>' + ev.when +
          '<br>Observed ' + fmtVal(r.value) + ', expected near ' + fmtVal(r.expected);
        mark.addEventListener('mouseenter', function (e2) {
          var wrap = host.parentElement;
          var box = wrap.getBoundingClientRect();
          showTip(tipHtml, e2.clientX - box.left, e2.clientY - box.top, wrap);
        });
        mark.addEventListener('mouseleave', hideTip);
        mark.addEventListener('focus', function () {
          announce(ev.title + ' at ' + ev.when + '. Card ' + ev.id + ' below has the full reading.');
          var wrap = host.parentElement;
          showTip(tipHtml, cx / W * wrap.clientWidth, cy / H * wrap.clientHeight, wrap);
        });
        mark.addEventListener('blur', hideTip);
        mark.addEventListener('keydown', function (e3) {
          if (e3.key === 'Enter' || e3.key === ' ') {
            e3.preventDefault();
            jumpToCard(ev.id);
          }
          if (e3.key === 'ArrowRight' || e3.key === 'ArrowLeft') {
            e3.preventDefault();
            var idx = focusables.indexOf(mark);
            var next = e3.key === 'ArrowRight' ? idx + 1 : idx - 1;
            if (next >= 0 && next < focusables.length) focusables[next].focus();
          }
        });
        mark.addEventListener('click', function () { jumpToCard(ev.id); });
        svg.appendChild(mark);
        focusables.push(mark);
      });
    }

    host.appendChild(svg);
  }

  function isOn(id) {
    var box = document.getElementById(id);
    return !box || box.checked;
  }

  /* Marker to card: scroll honors reduced motion and a short outline
     pulse shows which card answered without permanent decoration. */
  function jumpToCard(id) {
    var card = document.getElementById('card-' + id);
    if (!card) return;
    card.scrollIntoView({ behavior: reducedMotion() ? 'auto' : 'smooth', block: 'center' });
    card.style.outline = '3px solid var(--focus)';
    setTimeout(function () { card.style.outline = ''; }, 1600);
  }

  /* Card to marker: moving focus is the highlight, because focus rings
     are already the one visual state every palette keeps visible. */
  function highlightMarker(id) {
    var mark = document.querySelector('[data-event="' + id + '"]');
    if (mark && mark.focus) mark.focus();
  }

  /* Toolbar and card chip wiring, all delegated. */
  document.addEventListener('change', function (e) {
    var t = e.target;
    if (!t) return;
    if (['show-band', 'show-trend', 'show-marks'].indexOf(t.id) >= 0) render();
    if (t.id === 'focus-window') { state.focus = t.value; render(); }
  });
  document.addEventListener('click', function (e) {
    var chip = e.target.closest ? e.target.closest('.mark-jump') : null;
    if (chip) highlightMarker(chip.getAttribute('data-event'));
  });

  /* Handler registration retries until the Shiny object exists, because
     registering at script load races the socket boot and loses quietly. */
  function register() {
    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler('render-chart', function (payload) {
        state.payload = payload;
        if (window.NcModel) window.NcModel.setPayload(payload);
        render();
      });
    } else {
      setTimeout(register, 120);
    }
  }
  register();

  window.addEventListener('resize', function () { render(); });

  /* Exposed for the DOM test harness. */
  window.NcChart = {
    render: render,
    toRows: toRows,
    markerPath: markerPath,
    setPayload: function (p) { state.payload = p; }
  };
})();
