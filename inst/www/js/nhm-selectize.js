(function () {
  function portalSelectizeDropdown(instance) {
    if (!instance || !instance.$dropdown) {
      return;
    }

    instance.settings.dropdownParent = "body";

    if (instance.$dropdown.parent()[0] !== document.body) {
      instance.$dropdown.appendTo(document.body);
    }

    if (typeof instance.positionDropdown === "function") {
      instance.positionDropdown();
    }
  }

  function applyToAllSelectizeInputs() {
    var nodes = document.querySelectorAll(".selectized");
    nodes.forEach(function (node) {
      if (node && node.selectize) {
        portalSelectizeDropdown(node.selectize);
      }
    });
  }

  function initObserver() {
    var observer = new MutationObserver(function () {
      applyToAllSelectizeInputs();
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  document.addEventListener("DOMContentLoaded", function () {
    applyToAllSelectizeInputs();
    initObserver();
  });

  document.addEventListener("shown.bs.tab", applyToAllSelectizeInputs);

  document.addEventListener("click", function (event) {
    var control = event.target.closest(".selectize-control");
    if (!control) {
      return;
    }

    var source = control.previousElementSibling;
    if (source && source.selectize) {
      portalSelectizeDropdown(source.selectize);
    }
  });

  if (window.jQuery) {
    window.jQuery(document).on("shiny:connected shiny:value", applyToAllSelectizeInputs);
  }
})();
