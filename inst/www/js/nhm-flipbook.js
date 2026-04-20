// shinynhm flipbook navigation
(function() {
  'use strict';

  var initialized = {};
  var flipbooks   = {};  // fbId -> goToPage function, for hashchange

  function getHashParam(key) {
    var raw = location.hash.substring(1);  // strip leading #
    var match = raw.match(new RegExp('(?:^|&)' + key + '=(\\d+)'));
    return match ? parseInt(match[1], 10) : null;
  }

  function setHashParam(key, value) {
    var raw = location.hash.substring(1);
    // Remove existing key
    raw = raw.replace(new RegExp('(?:^|&)' + key + '=\\d+'), '');
    raw = raw.replace(/^&/, '');
    var updated = (raw ? raw + '&' : '') + key + '=' + value;
    // Use replaceState to avoid polluting browser history on every page turn
    history.replaceState(null, '', '#' + updated);
  }

  function initAll() {
    document.querySelectorAll('.nhm-flipbook').forEach(function(el) {
      var fbId = el.dataset.flipbookId || 'flipbook';
      if (initialized[fbId]) return;
      initialized[fbId] = true;
      initFlipbook(el, fbId);
    });
  }

  function initFlipbook(container, fbId) {
    var pages   = container.querySelectorAll('.nhm-flipbook-page');
    var nav     = container.querySelector('.nhm-flipbook-nav');
    if (!pages.length || !nav) return;

    var dotsEl  = nav.querySelector('.nhm-flipbook-dots');
    var labelEl = nav.querySelector('.nhm-flipbook-label');
    var prevBtn = nav.querySelector('.nhm-flipbook-prev');
    var nextBtn = nav.querySelector('.nhm-flipbook-next');
    var current = 0;

    // Restore page from URL hash (e.g. #main=3 -> page index 2)
    var restored = getHashParam(fbId);
    if (restored !== null) {
      var idx = restored - 1;
      if (idx >= 0 && idx < pages.length) {
        current = idx;
        pages[0].classList.remove('active');
      }
    }

    // Hide the regular footer when flipbook is active
    document.documentElement.classList.add('nhm-flipbook-active');

    // Clear any existing dots (in case of re-init)
    dotsEl.innerHTML = '';

    // Build dot indicators
    for (var i = 0; i < pages.length; i++) {
      var dot = document.createElement('button');
      dot.className = 'nhm-flipbook-dot' + (i === current ? ' active' : '');
      dot.setAttribute('aria-label', 'Go to page ' + (i + 1));
      dot.dataset.page = i;
      (function(idx) {
        dot.addEventListener('click', function() { goToPage(idx); });
      })(i);
      dotsEl.appendChild(dot);
    }

    function goToPage(idx, skipHash) {
      if (idx < 0 || idx >= pages.length || idx === current) return;
      pages[current].classList.remove('active');
      dotsEl.children[current].classList.remove('active');
      current = idx;
      pages[current].classList.add('active');
      dotsEl.children[current].classList.add('active');
      updateNav();

      // Persist page in URL hash
      if (!skipHash) {
        setHashParam(fbId, current + 1);
      }

      // Notify Shiny of page change
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue(fbId + '_page', current + 1);
      }

      // Trigger resize so Shiny recalculates plot dimensions
      // and fire 'shown' so Shiny un-suspends hidden outputs
      setTimeout(function() {
        if (typeof jQuery !== 'undefined') {
          jQuery(pages[current]).trigger('shown');
          jQuery(pages[current]).find('.shiny-plot-output').trigger('shown');
        }
        window.dispatchEvent(new Event('resize'));
      }, 100);
      setTimeout(function() {
        window.dispatchEvent(new Event('resize'));
      }, 400);

      // Scroll to top
      window.scrollTo({ top: 0, behavior: 'smooth' });
    }

    // Expose goToPage for hashchange handler
    flipbooks[fbId] = { goToPage: goToPage, getCurrent: function() { return current; } };

    function updateNav() {
      prevBtn.disabled = (current === 0);
      nextBtn.disabled = (current === pages.length - 1);
      var title = pages[current].dataset.title || '';
      labelEl.textContent = (current + 1) + ' / ' + pages.length +
        (title ? ' \u2014 ' + title : '');
    }

    prevBtn.addEventListener('click', function() { goToPage(current - 1); });
    nextBtn.addEventListener('click', function() { goToPage(current + 1); });

    // Keyboard navigation — only respond when focus is inside the flipbook
    document.addEventListener('keydown', function(e) {
      if (!container.contains(document.activeElement) &&
          !nav.contains(document.activeElement) &&
          document.activeElement !== document.body) return;
      if (!container.offsetParent && !container.offsetHeight) return;
      var tag = e.target.tagName;
      if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
      if (e.key === 'ArrowLeft')  { e.preventDefault(); goToPage(current - 1); }
      if (e.key === 'ArrowRight') { e.preventDefault(); goToPage(current + 1); }
    });

    updateNav();

    // If restored from hash, activate the correct page
    if (current > 0) {
      pages[current].classList.add('active');
      updateNav();
    }

    // Initial resize to size plots on the active page
    setTimeout(function() {
      if (typeof jQuery !== 'undefined') {
        jQuery(pages[current]).trigger('shown');
        jQuery(pages[current]).find('.shiny-plot-output').trigger('shown');
      }
      window.dispatchEvent(new Event('resize'));
    }, 200);
  }

  // Handle browser back/forward
  window.addEventListener('hashchange', function() {
    Object.keys(flipbooks).forEach(function(fbId) {
      var fb = flipbooks[fbId];
      var target = getHashParam(fbId);
      if (target !== null && (target - 1) !== fb.getCurrent()) {
        fb.goToPage(target - 1, true);
      }
    });
  });

  // Try multiple init strategies to handle different load timings
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initAll);
  } else {
    // DOM already loaded — run on next tick to let Shiny finish rendering
    setTimeout(initAll, 0);
  }

  // Also init when Shiny signals it's ready
  if (typeof jQuery !== 'undefined') {
    jQuery(document).on('shiny:sessioninitialized', function() {
      setTimeout(initAll, 100);
    });
  }
})();
