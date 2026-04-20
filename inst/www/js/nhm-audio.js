(function () {
  "use strict";

  function formatTime(s) {
    if (!isFinite(s)) return "0:00";
    var m = Math.floor(s / 60);
    var sec = Math.floor(s % 60);
    return m + ":" + (sec < 10 ? "0" : "") + sec;
  }

  function initPlayer(el) {
    var src = el.getAttribute("data-src");
    var audio = new Audio(src);
    audio.preload = "auto";

    var playBtn = el.querySelector(".nhm-audio-play");
    var seekBar = el.querySelector(".nhm-audio-seek");
    var seekFill = el.querySelector(".nhm-audio-seek-fill");
    var seekThumb = el.querySelector(".nhm-audio-seek-thumb");
    var curTime = el.querySelector(".nhm-audio-current");
    var durTime = el.querySelector(".nhm-audio-duration");
    var volBtn = el.querySelector(".nhm-audio-vol-btn");
    var volBar = el.querySelector(".nhm-audio-vol");
    var volFill = el.querySelector(".nhm-audio-vol-fill");
    var volThumb = el.querySelector(".nhm-audio-vol-thumb");
    var duration = 0;

    audio.volume = 0.8;
    if (volFill) volFill.style.width = "80%";
    if (volThumb) volThumb.style.left = "80%";

    function updateDuration() {
      if (audio.duration && isFinite(audio.duration)) {
        duration = audio.duration;
        durTime.textContent = formatTime(duration);
      }
    }

    audio.addEventListener("loadedmetadata", updateDuration);
    audio.addEventListener("durationchange", updateDuration);
    audio.addEventListener("canplay", updateDuration);

    audio.addEventListener("timeupdate", function () {
      curTime.textContent = formatTime(audio.currentTime);
      var dur = audio.duration;
      if (dur && isFinite(dur)) {
        var pct = (audio.currentTime / dur) * 100;
        seekFill.style.width = pct + "%";
        seekThumb.style.left = pct + "%";
      }
    });

    audio.addEventListener("ended", function () {
      playBtn.innerHTML = "&#9654;";
      playBtn.setAttribute("aria-label", "Play");
    });

    playBtn.addEventListener("click", function () {
      if (audio.paused) {
        audio.play();
        playBtn.innerHTML = "&#9646;&#9646;";
        playBtn.setAttribute("aria-label", "Pause");
      } else {
        audio.pause();
        playBtn.innerHTML = "&#9654;";
        playBtn.setAttribute("aria-label", "Play");
      }
    });

    function seekFromEvent(e) {
      var dur = audio.duration;
      if (!dur || !isFinite(dur)) return;
      var rect = seekBar.getBoundingClientRect();
      var pct = Math.max(0, Math.min(1, (e.clientX - rect.left) / rect.width));
      audio.currentTime = pct * dur;
    }

    var seeking = false;
    seekBar.addEventListener("mousedown", function (e) {
      e.preventDefault();
      e.stopPropagation();
      seeking = true;
      seekFromEvent(e);
    });
    seekBar.addEventListener("click", function (e) {
      e.preventDefault();
      e.stopPropagation();
      seekFromEvent(e);
    });
    document.addEventListener("mousemove", function (e) {
      if (seeking) {
        e.preventDefault();
        seekFromEvent(e);
      }
    });
    document.addEventListener("mouseup", function () {
      seeking = false;
    });

    if (volBtn) {
      volBtn.addEventListener("click", function () {
        audio.muted = !audio.muted;
        volBtn.innerHTML = audio.muted ? "&#128263;" : "&#128265;";
        volBtn.setAttribute("aria-label", audio.muted ? "Unmute" : "Mute");
      });
    }

    function setVolume(pct) {
      pct = Math.max(0, Math.min(1, pct));
      audio.volume = pct;
      audio.muted = false;
      if (volFill) volFill.style.width = (pct * 100) + "%";
      if (volThumb) volThumb.style.left = (pct * 100) + "%";
      if (volBtn) {
        volBtn.innerHTML = pct === 0 ? "&#128263;" : "&#128265;";
      }
    }

    function volFromEvent(e) {
      var rect = volBar.getBoundingClientRect();
      var pct = Math.max(0, Math.min(1, (e.clientX - rect.left) / rect.width));
      setVolume(pct);
    }

    var draggingVol = false;
    if (volBar) {
      volBar.addEventListener("mousedown", function (e) {
        e.preventDefault();
        e.stopPropagation();
        draggingVol = true;
        volFromEvent(e);
      });
      document.addEventListener("mousemove", function (e) {
        if (draggingVol) {
          e.preventDefault();
          volFromEvent(e);
        }
      });
      document.addEventListener("mouseup", function () {
        draggingVol = false;
      });
    }

    // Options menu — portal to body for z-index safety
    var optionsBtn = el.querySelector(".nhm-audio-options-btn");
    var optionsMenu = el.querySelector(".nhm-audio-options-menu");
    if (optionsBtn && optionsMenu) {
      document.body.appendChild(optionsMenu);

      function positionMenu() {
        var rect = optionsBtn.getBoundingClientRect();
        var menuH = optionsMenu.offsetHeight;
        var spaceBelow = window.innerHeight - rect.bottom;
        if (spaceBelow >= menuH + 4) {
          optionsMenu.style.top = (rect.bottom + 2 + window.scrollY) + "px";
        } else {
          optionsMenu.style.top = (rect.top - menuH - 2 + window.scrollY) + "px";
        }
        optionsMenu.style.left = "";
        optionsMenu.style.right = "";
        var menuW = optionsMenu.offsetWidth;
        var leftPos = rect.right - menuW;
        if (leftPos < 4) leftPos = 4;
        optionsMenu.style.left = leftPos + "px";
      }

      optionsBtn.addEventListener("click", function (e) {
        e.stopPropagation();
        var open = optionsMenu.classList.toggle("open");
        optionsBtn.setAttribute("aria-expanded", open);
        if (open) positionMenu();
      });
      document.addEventListener("click", function () {
        optionsMenu.classList.remove("open");
        optionsBtn.setAttribute("aria-expanded", "false");
      });
      optionsMenu.addEventListener("click", function (e) {
        e.stopPropagation();
      });
      window.addEventListener("resize", function () {
        if (optionsMenu.classList.contains("open")) positionMenu();
      });
    }

    // Speed buttons
    var speedBtns = el.querySelectorAll(".nhm-audio-speed-btn");
    speedBtns.forEach(function (btn) {
      btn.addEventListener("click", function () {
        audio.playbackRate = parseFloat(btn.getAttribute("data-speed"));
        speedBtns.forEach(function (b) { b.classList.remove("active"); });
        btn.classList.add("active");
      });
    });
  }

  // Initialise any player that appears (including Shiny re-renders)
  var observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      m.addedNodes.forEach(function (node) {
        if (node.nodeType !== 1) return;
        if (node.classList && node.classList.contains("nhm-audio-player")) {
          initPlayer(node);
        }
        var children = node.querySelectorAll && node.querySelectorAll(".nhm-audio-player");
        if (children) {
          children.forEach(function (child) { initPlayer(child); });
        }
      });
    });
  });

  if (document.body) {
    observer.observe(document.body, { childList: true, subtree: true });
  } else {
    document.addEventListener("DOMContentLoaded", function () {
      observer.observe(document.body, { childList: true, subtree: true });
    });
  }

  // Init any already in the DOM
  document.addEventListener("DOMContentLoaded", function () {
    document.querySelectorAll(".nhm-audio-player").forEach(initPlayer);
  });
})();
