#' Create an NHM-styled slide-out panel
#'
#' A panel that slides in from the right edge of the screen when a fixed
#' tab button is clicked, matching the reference NHM dashboard design.
#'
#' @param ... UI elements to place inside the panel.
#' @param id Character. A unique identifier for the panel.
#' @param tab_label Character. Text shown on the vertical tab button.
#'   Defaults to \code{"More info"}.
#' @param width CSS width for the panel. Defaults to \code{"420px"}.
#' @param top CSS top position for the tab button. Defaults to
#'   \code{"50\%"}. Use different values for multiple panels.
#'
#' @return A \code{shiny.tag.list} containing the tab button, panel,
#'   and toggle script.
#' @export
nhm_slide_panel <- function(...,
                            id = "slide_panel",
                            tab_label = "More info",
                            width = "420px",
                            top = "50%") {

  tab_id   <- paste0(id, "_tab")
  panel_id <- paste0(id, "_panel")
  width_px <- width

  js <- sprintf(
    "$(document).on('shiny:sessioninitialized', function() {
       var tab = document.getElementById('%s');
       var panel = document.getElementById('%s');
       if (tab && panel) {
         panel.setAttribute('inert', '');
         function closePanel(p, t, w) {
           p.classList.remove('open');
           t.classList.remove('panel-open');
           p.style.right = '-' + w;
           t.setAttribute('aria-expanded', 'false');
           p.setAttribute('inert', '');
         }
         function togglePanel() {
           var opening = !panel.classList.contains('open');
           if (opening) {
             document.querySelectorAll('.nhm-slide-panel.open').forEach(function(op) {
               var ot = document.querySelector('[aria-controls=\"' + op.id + '\"]');
               if (ot && op !== panel) closePanel(op, ot, op.style.width);
             });
           }
           var isOpen = panel.classList.toggle('open');
           tab.classList.toggle('panel-open');
           panel.style.right = isOpen ? '0' : '-%s';
           tab.setAttribute('aria-expanded', isOpen);
           if (isOpen) {
             panel.removeAttribute('inert');
             panel.focus();
           } else {
             panel.setAttribute('inert', '');
             tab.focus();
           }
         }
         tab.addEventListener('click', togglePanel);
         document.addEventListener('keydown', function(e) {
           if (e.key === 'Escape' && panel.classList.contains('open')) {
             togglePanel();
           }
         });
       }
     });",
    tab_id, panel_id, width_px
  )

  shiny::tagList(
    shiny::tags$button(
      class             = "nhm-slide-tab",
      id                = tab_id,
      style             = paste0("top:", top),
      `aria-expanded`   = "false",
      `aria-controls`   = panel_id,
      tab_label
    ),
    shiny::tags$div(
      class          = "nhm-slide-panel",
      id             = panel_id,
      role           = "region",
      `aria-label`   = tab_label,
      tabindex       = "-1",
      style = paste0(
        "width:", width_px, ";",
        "right:-", width_px, ";"
      ),
      ...
    ),
    shiny::tags$style(shiny::HTML(sprintf(
      ".nhm-slide-tab.panel-open#%s { right: %s; }", tab_id, width_px
    ))),
    shiny::tags$script(shiny::HTML(js))
  )
}
