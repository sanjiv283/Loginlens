/* LoginLens — app.js: sidebar nav sync + UI enhancements */
(function () {
  "use strict";

  /* ── Sidebar navigation active state ─────────────────── */
  function syncNav(tabName) {
    document.querySelectorAll(".ll-nav-item").forEach(function (el) {
      var matches = el.getAttribute("data-tab") === tabName;
      el.classList.toggle("active", matches);
    });
    updateHeader(tabName);
  }

  var TAB_META = {
    overview: {
      title:    "OVERVIEW",
      subtitle: "System telemetry and anomaly summary",
    },
    explorer: {
      title:    "LOGIN EXPLORER",
      subtitle: "Forensic investigation interface — evaluation period",
    },
    threshold: {
      title:    "THRESHOLD LAB",
      subtitle: "Detection profile configuration and metric comparison",
    },
    insights: {
      title:    "STATISTICAL INSIGHTS",
      subtitle: "Methodology, assumptions and model performance",
    },
  };

  function updateHeader(tabName) {
    var meta = TAB_META[tabName] || TAB_META["overview"];
    var titleEl    = document.getElementById("ll-page-title-text");
    var subtitleEl = document.getElementById("ll-page-subtitle-text");
    if (titleEl)    titleEl.textContent    = meta.title;
    if (subtitleEl) subtitleEl.textContent = meta.subtitle;
  }

  /* ── Click handler for sidebar nav items ─────────────── */
  function attachNavHandlers() {
    document.querySelectorAll(".ll-nav-item").forEach(function (el) {
      el.addEventListener("click", function () {
        var tabName = el.getAttribute("data-tab");
        if (!tabName) return;

        // Map our tab keys to Shiny's tab panel anchor link text
        var tabLabelMap = {
          overview:  "Overview",
          explorer:  "Login Explorer",
          threshold: "Threshold Lab",
          insights:  "Statistical Insights",
        };
        var label = tabLabelMap[tabName];
        if (!label) return;

        // Find and click the corresponding Shiny tab link
        var shinyTabs = document.querySelectorAll("ul.nav-tabs > li > a, .nav-tabs a[data-value]");
        shinyTabs.forEach(function (link) {
          if (link.textContent.trim() === label || link.getAttribute("data-value") === label) {
            link.click();
          }
        });

        syncNav(tabName);
      });
    });
  }

  /* ── Observe Shiny tab changes (via MutationObserver) ── */
  function observeTabChanges() {
    var tabContent = document.querySelector(".tab-content");
    if (!tabContent) return;

    var observer = new MutationObserver(function () {
      var active = tabContent.querySelector(".tab-pane.active");
      if (!active) return;
      var id = active.getAttribute("data-value") || active.getAttribute("id") || "";
      // Convert Shiny label to our key
      var map = {
        "Overview":            "overview",
        "Login Explorer":      "explorer",
        "Threshold Lab":       "threshold",
        "Statistical Insights":"insights",
      };
      var key = map[id] || null;
      if (key) syncNav(key);
    });

    observer.observe(tabContent, { attributes: true, subtree: true, attributeFilter: ["class"] });
  }

  /* ── Animate KPI values on tab show ──────────────────── */
  function animateKpi() {
    document.querySelectorAll(".ll-kpi-value").forEach(function (el) {
      el.style.opacity = "0";
      el.style.transform = "translateY(8px)";
      el.style.transition = "opacity 400ms ease, transform 400ms ease";
      setTimeout(function () {
        el.style.opacity = "1";
        el.style.transform = "translateY(0)";
      }, 80);
    });
  }

  /* ── Init ─────────────────────────────────────────────── */
  function init() {
    attachNavHandlers();
    observeTabChanges();
    syncNav("overview"); // default active tab
    animateKpi();

    // Re-animate KPI when Shiny re-renders
    $(document).on("shiny:value", function (e) {
      if (["n_events", "n_users", "n_flagged", "pct_flagged"].indexOf(e.name) !== -1) {
        animateKpi();
      }
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
