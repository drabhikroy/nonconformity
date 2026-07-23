/* Theme and palette state for Nonconformity.
   Everything here is client side on purpose: appearance must respond on
   the first paint and keep working even if the Shiny socket is still
   waking up, so no server round trip is involved at any point. */

(function () {
  var PALETTES = ['cb-deutan', 'cb-tritan', 'cb-mono'];

  /* localStorage can throw in locked down or private contexts, so both
     helpers swallow the failure and fall back to session defaults. */
  function saved(key) {
    try { return localStorage.getItem(key); } catch (e) { return null; }
  }
  function store(key, val) {
    try { localStorage.setItem(key, val); } catch (e) {}
  }

  /* Dark is the default appearance. A stored light choice wins over it,
     which keeps the choice stable across sessions on the same machine. */
  function applyTheme() {
    var choice = saved('nonconformity-theme');
    if (choice === 'light') document.body.classList.remove('dark');
    else document.body.classList.add('dark');
    refreshThemeLabel();
  }

  /* The toggle names the mode it would switch to, not the current one,
     which is the reading people expect from a mode button. */
  function refreshThemeLabel() {
    var btn = document.getElementById('theme-toggle');
    if (btn) {
      btn.textContent = document.body.classList.contains('dark')
        ? 'Light mode' : 'Dark mode';
    }
  }

  /* One body class per palette; the stylesheet swaps every meaningful
     color at once, and pressed states mirror the choice for readers. */
  function applyPalette(p) {
    PALETTES.forEach(function (c) { document.body.classList.remove(c); });
    if (p && p !== 'standard') document.body.classList.add(p);
    var btns = document.querySelectorAll('.seg-btn[data-palette]');
    btns.forEach(function (b) {
      var on = (b.getAttribute('data-palette') === (p || 'standard'));
      b.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
  }

  /* Delegated clicks survive any re render Shiny performs, because the
     listener sits on the document rather than on the buttons. */
  document.addEventListener('click', function (e) {
    var t = e.target;
    var themeBtn = t.closest ? t.closest('#theme-toggle') : null;
    if (themeBtn) {
      document.body.classList.toggle('dark');
      store('nonconformity-theme',
        document.body.classList.contains('dark') ? 'dark' : 'light');
      refreshThemeLabel();
      return;
    }
    var seg = t.closest ? t.closest('.seg-btn[data-palette]') : null;
    if (seg) {
      var p = seg.getAttribute('data-palette');
      applyPalette(p);
      store('nonconformity-palette', p);
    }
    var per = t.closest ? t.closest('.persona-btn[data-persona]') : null;
    if (per) {
      applyPersona(per.getAttribute('data-persona'));
    }
  });

  /* Persona is a display mode, so the body class carries it and the
     stylesheet does the revealing. The server hears about it only because
     the method list differs between the two views. */
  function applyPersona(name) {
    var researcher = (name === 'researcher');
    document.body.classList.toggle('show-researcher', researcher);
    document.querySelectorAll('.persona-btn[data-persona]').forEach(function (b) {
      b.setAttribute('aria-pressed',
        b.getAttribute('data-persona') === name ? 'true' : 'false');
    });
    store('nonconformity-persona', name);
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue('persona', name, { priority: 'event' });
    }
  }

  /* Boot runs as early as the document allows so the first paint already
     shows the stored appearance without a flash of the wrong theme. */
  function boot() {
    applyTheme();
    applyPalette(saved('nonconformity-palette') || 'standard');
    applyPersona(saved('nonconformity-persona') || 'operator');
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }

  /* Exposed for the DOM test harness. */
  window.NcUi = { applyPalette: applyPalette, applyTheme: applyTheme,
                  applyPersona: applyPersona };
})();
