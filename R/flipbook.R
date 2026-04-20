#' Create a flipbook page container
#'
#' A single page within an \code{nhm_flipbook}. Only the active page is
#' visible; users navigate between pages with prev/next buttons and dot
#' indicators.
#'
#' @param ... UI elements to display on this page.
#' @param title Character. The page title shown in the bottom navigation
#'   label (e.g. "1 / 5 — The Initiative").
#'
#' @return A \code{shiny.tag} div element.
#' @export
nhm_flipbook_page <- function(..., title = "") {
  shiny::tags$div(
    class       = "nhm-flipbook-page",
    `data-title` = title,
    shiny::div(class = "container-fluid", ...)
  )
}


#' Create a flipbook navigation component
#'
#' Wraps multiple \code{nhm_flipbook_page} elements into a full-page
#' flipbook with prev/next arrow buttons and dot indicators in a fixed
#' bottom navigation bar, matching the NHM UHI dashboard style.
#'
#' The first page is shown by default. Users can navigate with the
#' arrow buttons, clicking dots, or using keyboard arrow keys.
#'
#' @param ... One or more \code{nhm_flipbook_page()} elements.
#' @param id Character. A unique ID for the flipbook. The current page
#'   number (1-indexed) is available as a Shiny input via
#'   \code{input$<id>_page}.
#'
#' @return A \code{shiny.tag} div element.
#' @export
#'
#' @examples
#' \dontrun{
#' nhm_flipbook(
#'   id = "main",
#'   nhm_flipbook_page(title = "Overview",  h2("Page 1"), p("Hello")),
#'   nhm_flipbook_page(title = "Analysis",  h2("Page 2"), plotOutput("p")),
#'   nhm_flipbook_page(title = "Thank You", h2("Thanks!"))
#' )
#' }
nhm_flipbook <- function(..., id = "flipbook") {
  pages <- list(...)

  # Mark the first page as active
  if (length(pages) > 0) {
    pages[[1]] <- htmltools::tagAppendAttributes(
      pages[[1]], class = "active"
    )
  }

  js_dep <- htmltools::htmlDependency(
    name    = "nhm-flipbook",
    version = "0.1.0",
    src     = system.file("www", package = "shinynhm"),
    script  = "js/nhm-flipbook.js"
  )

  shiny::tags$div(
    class              = "nhm-flipbook",
    `data-flipbook-id` = id,
    js_dep,
    shiny::tags$div(class = "nhm-flipbook-pages", pages),
    shiny::tags$div(
      class = "nhm-flipbook-nav",
      shiny::tags$div(
        class = "nhm-flipbook-controls",
        shiny::tags$button(
          class = "nhm-flipbook-btn nhm-flipbook-prev",
          `aria-label` = "Previous page",
          shiny::HTML("&#8592;")
        ),
        shiny::tags$div(class = "nhm-flipbook-dots"),
        shiny::tags$button(
          class = "nhm-flipbook-btn nhm-flipbook-next",
          `aria-label` = "Next page",
          shiny::HTML("&#8594;")
        )
      ),
      shiny::tags$span(
        class       = "nhm-flipbook-label",
        role        = "status",
        `aria-live` = "polite"
      )
    )
  )
}
