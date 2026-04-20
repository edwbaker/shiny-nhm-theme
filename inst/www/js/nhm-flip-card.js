// shinynhm flip card — unobtrusive event handlers
(function() {
  'use strict';

  function initFlipCards() {
    document.querySelectorAll('.nhm-flip-card').forEach(function(card) {
      if (card.dataset.flipInit) return;
      card.dataset.flipInit = '1';

      card.addEventListener('click', function() {
        toggle(card);
      });

      card.addEventListener('keydown', function(e) {
        if (e.key === 'Enter' || e.key === ' ') {
          e.preventDefault();
          toggle(card);
        }
      });
    });
  }

  function toggle(card) {
    card.classList.toggle('flipped');
    var expanded = card.classList.contains('flipped');
    card.setAttribute('aria-expanded', expanded);

    // Toggle aria-hidden on front/back faces
    var front = card.querySelector('.nhm-flip-card-front');
    var back  = card.querySelector('.nhm-flip-card-back');
    if (front) front.setAttribute('aria-hidden', expanded);
    if (back)  back.setAttribute('aria-hidden', !expanded);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initFlipCards);
  } else {
    setTimeout(initFlipCards, 0);
  }

  if (typeof jQuery !== 'undefined') {
    jQuery(document).on('shiny:sessioninitialized', function() {
      setTimeout(initFlipCards, 100);
    });
  }
})();
