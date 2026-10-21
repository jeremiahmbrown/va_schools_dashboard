(function () {
  function ready() {
    var stage = document.getElementById("detail-stage");
    var toggle = document.getElementById("sheet-toggle");
    var menu = document.querySelector(".filter-menu");
    var closeFilters = document.getElementById("filter-close");
    if (!stage || !toggle) return;

    function setOpen(open) {
      stage.classList.toggle("is-open", open);
      toggle.setAttribute("aria-expanded", String(open));
      toggle.textContent = open ? "Show more map" : "View details";
    }

    toggle.addEventListener("click", function () {
      setOpen(!stage.classList.contains("is-open"));
    });

    if (window.Shiny) {
      Shiny.addCustomMessageHandler("dashboard-selection-changed", function (_message) {
        if (window.matchMedia("(max-width: 760px)").matches) setOpen(false);
      });
    }

    if (menu && closeFilters) {
      closeFilters.addEventListener("click", function () {
        menu.open = false;
        menu.querySelector("summary").focus({ preventScroll: true });
      });
    }

    // A floating filter menu must not cover the map after another control is used.
    document.addEventListener("click", function (event) {
      if (menu && menu.open && !menu.contains(event.target)) menu.open = false;
    });

    document.addEventListener("keydown", function (event) {
      if (event.key === "Escape" && menu && menu.open) {
        menu.open = false;
        menu.querySelector("summary").focus({ preventScroll: true });
      }
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", ready);
  else ready();
})();
