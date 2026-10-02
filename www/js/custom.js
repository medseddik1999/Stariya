// petit helper : focus auto sur les inputs de recherche
$(document).on('shiny:connected', function () {
  // rien de spécial pour l'instant — place aux futures interactions
});

$(document).on('click', '.sidebar-menu a', function () {
  if (window.innerWidth <= 767) {
    $('body').removeClass('sidebar-open');
  }
});

Shiny.addCustomMessageHandler('startup-os-navigation', function (allowedTabs) {
  Object.entries(allowedTabs).forEach(function ([tab, allowed]) {
    const link = document.querySelector('.sidebar-menu a[data-value="' + tab + '"]');
    if (link) {
      link.closest('li').hidden = !allowed;
    }
  });
  const active = document.querySelector('.sidebar-menu li.active a');
  if (active && active.closest('li').hidden) {
    const firstAllowed = document.querySelector('.sidebar-menu li:not([hidden]) a[data-value]');
    if (firstAllowed) firstAllowed.click();
  }
});