(function () {
  function ready() {
    var stage = document.getElementById("detail-stage");
    var toggle = document.getElementById("sheet-toggle");
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
      Shiny.addCustomMessageHandler("dashboard-selection-changed", function () {
        if (window.matchMedia("(max-width: 760px)").matches) setOpen(false);
      });
    }

    // A floating filter menu must not cover the map after another control is used.
    document.addEventListener("click", function (event) {
      var menu = document.querySelector(".filter-menu");
      if (menu && menu.open && !menu.contains(event.target)) menu.open = false;
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", ready);
  else ready();
})();
