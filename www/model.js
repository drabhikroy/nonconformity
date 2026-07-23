/* Nonconformity local model layer.
   The browser talks to Ollama on the same machine directly, so no series
   data touches the Shiny server for this feature, let alone the network.
   Everything here is additive: the deterministic analysis is always
   complete on its own, and every model output is clearly labeled as such.

   The model is given the analysis, not the raw readings. It receives the
   summary line, the method, and the event list with sizes and timings, so
   its context is small, its calls are fast, and it never sees the data it
   might otherwise leak or memorize. */

(function () {
  var models = [];
  var payload = null;
  var chatHistory = [];

  /* The address box is the single source of truth for the endpoint, and a
     trailing slash is trimmed so path joins stay predictable. */
  function endpoint() {
    var box = document.getElementById('ollama-url');
    var url = box ? box.value.trim() : 'http://localhost:11434';
    return url.replace(/\/+$/, '');
  }

  /* All connection progress lands in one status line with a role of status,
     so screen readers hear results without a focus jump. */
  function status(text) {
    var s = document.getElementById('model-status');
    if (s) s.textContent = text;
  }

  function chosenModel() {
    var pick = document.getElementById('model-pick');
    return pick && pick.value ? pick.value : models[0];
  }

  function escapeHtml(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  /* The tags call doubles as the health check: it proves the server is up
     and lists what is pulled, which drives the whole card state. */
  function checkConnection() {
    status('Checking...');
    fetch(endpoint() + '/api/tags')
      .then(function (r) { return r.json(); })
      .then(function (data) {
        models = (data.models || []).map(function (m) { return m.name; });
        var pick = document.getElementById('model-pick');
        var actions = document.getElementById('model-actions');
        if (models.length === 0) {
          status('Connected, but no models are pulled yet. The guide shows the pull command.');
          return;
        }
        pick.innerHTML = models.map(function (m) {
          return '<option value="' + escapeHtml(m) + '">' + escapeHtml(m) + '</option>';
        }).join('');
        pick.style.display = 'block';
        if (actions) actions.style.display = 'block';
        revealChat();
        status('Connected. ' + models.length + ' model' +
          (models.length > 1 ? 's' : '') + ' available.');
      })
      .catch(function () {
        status('No answer at that address. Is Ollama running? The guide under the header walks through setup.');
      });
  }

  /* A compact digest of the analysis shared by every prompt, so the model
     always reasons from the same numbers the person is looking at. */
  function analysisContext() {
    if (!payload) return 'No data is loaded yet.';
    var m = payload.meta || {};
    var events = window.NcChart.toRows(payload.events);
    var lines = events.map(function (ev) {
      var size = Math.abs(ev.peak_score).toFixed(1);
      var extra = ev.type === 'run' ? (', lasting ' + ev.len + ' readings') :
        (ev.type === 'shift' ? (', level ' + fmt(ev.before_level) + ' to ' + fmt(ev.after_level)) : '');
      return '- ' + ev.title + ' at ' + ev.when + ' (' + ev.direction +
        ', about ' + size + ' scaled units' + extra + ')';
    }).join('\n');
    return 'Series: ' + (m.source || 'uploaded data') + ', ' + m.cadence +
      ' cadence, ' + (m.n || '?') + ' readings.\n' +
      'Method: ' + (m.method_label || m.method) + '. Sensitivity ' + m.sens +
      ' scaled units.\n' +
      'Deterministic summary: ' + (m.summary || '') + '\n' +
      'Events found (' + events.length + '):\n' + (lines || '- none');
  }

  function fmt(v) {
    if (v === null || v === undefined || isNaN(v)) return '?';
    return (Math.abs(v) >= 1000) ? Math.round(v).toLocaleString()
      : (Math.round(v * 100) / 100).toString();
  }

  /* Ollama answers are reported back as (text, failed) so callers can tell
     a machine that never answered from a model that answered with nothing.
     Collapsing the two produces the same message for a stopped server and
     a bad prompt, which sends people looking in the wrong place. */
  function generate(prompt, onResult, onDone) {
    fetch(endpoint() + '/api/generate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ model: chosenModel(), prompt: prompt, stream: false })
    })
      .then(function (r) {
        if (!r.ok) throw new Error('http ' + r.status);
        return r.json();
      })
      .then(function (data) {
        onResult(data.response || '', false);
        if (onDone) onDone();
      })
      .catch(function () {
        onResult('', true);
        if (onDone) onDone();
      });
  }

  /* Triage and the incident draft share one output card, so whichever
     produced the text also sets the heading. A fixed heading would label
     an incident writeup as a summary. */
  function showOutput(heading, label, text, failed) {
    var card = document.getElementById('triage-card');
    var title = document.getElementById('triage-title');
    var body = document.getElementById('triage-body');
    if (card) card.style.display = 'block';
    if (title) title.textContent = heading;
    if (!body) return;
    var note;
    if (failed) {
      note = 'The model could not be reached. Check that Ollama is still running.';
    } else if (!text) {
      note = 'The model answered with nothing. Try another model or a smaller run.';
    } else {
      note = escapeHtml(text).replace(/\n/g, '<br>');
    }
    body.innerHTML = '<span class="model-tag">' + escapeHtml(label) + '</span>' + note;
  }

  function triage() {
    if (!payload) { status('Load data first.'); return; }
    showOutput('Model summary of this run', 'Working...', '', false);
    status('Summarizing with ' + chosenModel() + '...');
    var prompt = 'You are a reliability analyst reading an anomaly detection run. ' +
      'Using only the information below, write three or four sentences for an ' +
      'operator: which one or two events deserve attention first and why, and ' +
      'whether the overall picture looks routine or worth a closer look. Plain ' +
      'language, no preamble, no markdown.\n\n' + analysisContext();
    generate(prompt, function (text, failed) {
      showOutput('Model summary of this run',
        'Model summary from ' + chosenModel(), text, failed);
      status(failed ? 'The model could not be reached.' : 'Summary ready.');
    });
  }

  /* Per event notes, one sequential call each so a small machine stays
     responsive rather than being flooded with parallel generations. */
  function annotate() {
    if (!payload) { status('Load data first.'); return; }
    var events = window.NcChart.toRows(payload.events);
    if (events.length === 0) { status('No events to annotate.'); return; }
    var cadence = (payload.meta && payload.meta.cadence) || 'regular';
    status('Writing notes with ' + chosenModel() + '...');
    var done = 0;
    var failed = 0;
    function next(i) {
      if (i >= events.length) {
        status(failed > 0
          ? ('Added ' + done + ' of ' + events.length + ' notes. ' + failed +
             ' call' + (failed === 1 ? '' : 's') + ' did not reach the model.')
          : ('Done. ' + done + ' note' + (done === 1 ? '' : 's') + ' added.'));
        return;
      }
      var ev = events[i];
      var prompt = 'You are annotating one anomaly in an operational time series (' +
        cadence + ' cadence). Event: ' + ev.title + ' at ' + ev.when +
        ', size about ' + Math.abs(ev.peak_score).toFixed(1) +
        ' scaled units, direction ' + ev.direction +
        '. In two sentences and under 45 words, name the two or three most ' +
        'common operational causes for this kind of event and one check a ' +
        'reviewer could run next. No preamble.';
      generate(prompt, function (text, callFailed) {
        if (callFailed) failed += 1;
        var note = document.getElementById('note-' + ev.id);
        if (note && text) {
          note.innerHTML = '<span class="model-tag">Model note from ' +
            escapeHtml(chosenModel()) + '</span>' + escapeHtml(text);
          note.style.display = 'block';
          done += 1;
        }
      }, function () { next(i + 1); });
    }
    next(0);
  }

  /* Incident draft: a longer single call producing copyable prose for a
     ticket. It lands in the same output card as the triage summary, under
     its own heading. */
  function incident() {
    if (!payload) { status('Load data first.'); return; }
    showOutput('Draft incident writeup', 'Working...', '', false);
    status('Drafting an incident writeup with ' + chosenModel() + '...');
    var prompt = 'You are writing a short incident note from an anomaly ' +
      'detection run. Produce four short paragraphs with no markdown: what was ' +
      'observed, when it happened and how large it was, the most likely ' +
      'operational causes, and a recommended next step. Ground every claim in ' +
      'the information below and do not invent specifics.\n\n' + analysisContext();
    generate(prompt, function (text, failed) {
      showOutput('Draft incident writeup',
        'Draft incident writeup from ' + chosenModel(), text, failed);
      status(failed ? 'The model could not be reached.'
                    : 'Incident draft ready above the reading cards.');
    });
  }

  /* Chat. The analysis context leads every exchange, and a short rolling
     history keeps follow up questions coherent without unbounded growth. */
  function revealChat() {
    var card = document.getElementById('chat-card');
    if (card) card.style.display = 'block';
  }

  function appendTurn(who, text, cls) {
    var log = document.getElementById('chat-log');
    if (!log) return null;
    var div = document.createElement('div');
    div.className = 'chat-turn ' + cls;
    div.innerHTML = '<span class="who">' + who + '</span>' + escapeHtml(text);
    log.appendChild(div);
    log.scrollTop = log.scrollHeight;
    return div;
  }

  function ask() {
    var input = document.getElementById('chat-input');
    if (!input || !input.value.trim()) return;
    if (!payload) { status('Load data first.'); return; }
    var q = input.value.trim();
    input.value = '';
    appendTurn('You', q, 'you');
    var pending = appendTurn(chosenModel(), 'Thinking...', 'model');
    var history = chatHistory.slice(-4).map(function (h) {
      return h.q + '\n' + h.a;
    }).join('\n\n');
    var prompt = 'You are answering a question about one anomaly detection run. ' +
      'Use only the information below. If the answer is not present, say so ' +
      'plainly rather than guessing. Keep it under 80 words, no markdown.\n\n' +
      analysisContext() +
      (history ? ('\n\nEarlier in this chat:\n' + history) : '') +
      '\n\nQuestion: ' + q;
    generate(prompt, function (text, failed) {
      var ans = failed
        ? 'The model could not be reached. Check that Ollama is still running.'
        : (text || 'The model answered with nothing. Try rephrasing the question.');
      if (pending) pending.innerHTML = '<span class="who">' +
        escapeHtml(chosenModel()) + '</span>' + escapeHtml(ans);
      chatHistory.push({ q: q, a: ans });
      var log = document.getElementById('chat-log');
      if (log) log.scrollTop = log.scrollHeight;
    });
  }

  document.addEventListener('click', function (e) {
    if (!e.target) return;
    if (e.target.id === 'model-check') checkConnection();
    if (e.target.id === 'model-triage') triage();
    if (e.target.id === 'model-notes') annotate();
    if (e.target.id === 'model-incident') incident();
    if (e.target.id === 'chat-send') ask();
  });
  document.addEventListener('keydown', function (e) {
    if (e.target && e.target.id === 'chat-input' && e.key === 'Enter') ask();
  });

  window.NcModel = {
    checkConnection: checkConnection,
    triage: triage,
    annotate: annotate,
    incident: incident,
    ask: ask,
    setPayload: function (p) { payload = p; }
  };
})();
