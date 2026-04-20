#' Animated timeline slider input
#'
#' Creates a Shiny slider with play/pause controls that automatically
#' steps through values at a configurable interval. Styled to match the
#' NHM dark theme.
#'
#' @param inputId The input slot name.
#' @param label Display label (or \code{NULL} for no label).
#' @param values A vector of values to step through (e.g.
#'   \code{2026:2070}). Can be numeric or character.
#' @param selected Initial selected value. Defaults to the first value.
#' @param interval Milliseconds between steps when playing. Default 800.
#' @param loop Logical. Whether to loop back to the start after
#'   reaching the end. Default \code{TRUE}.
#' @param palette Character. Passed to \code{\link{nhm_colours}} for
#'   button styling.
#' @param width CSS width. Defaults to \code{"100\%"}.
#'
#' @return A Shiny UI element.
#' @export
nhm_timeline_input <- function(inputId, label = NULL,
                               values, selected = values[1],
                               interval = 800, loop = TRUE,
                               palette = "default",
                               width = "100%") {
  cols <- nhm_colours(palette)

  slider <- shiny::sliderInput(
    inputId  = inputId,
    label    = label,
    min      = min(values),
    max      = max(values),
    value    = selected,
    step     = if (is.numeric(values)) min(diff(sort(unique(values)))) else 1,
    sep      = "",
    animate  = shiny::animationOptions(
      interval = interval,
      loop     = loop,
      playButton  = shiny::tags$button(
        class = "nhm-timeline-btn",
        style = paste0(
          "background:", cols$cyan, ";",
          "color:", cols$deep, ";",
          "border:none; border-radius:4px;",
          "padding:6px 16px; cursor:pointer;",
          "font-weight:700; font-size:0.85rem;"
        ),
        shiny::icon("play"), " Play"
      ),
      pauseButton = shiny::tags$button(
        class = "nhm-timeline-btn",
        style = paste0(
          "background:", cols$pink, ";",
          "color:", cols$deep, ";",
          "border:none; border-radius:4px;",
          "padding:6px 16px; cursor:pointer;",
          "font-weight:700; font-size:0.85rem;"
        ),
        shiny::icon("pause"), " Pause"
      )
    ),
    width = width
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .nhm-timeline .slider-animate-container {
        margin-top: 12px;
      }
    ")),
    shiny::tags$div(
      class = "nhm-timeline",
      slider
    )
  )
}
