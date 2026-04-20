#' Create an NHM-styled flip card
#'
#' A card with a front and back side that flips on click, matching the
#' reference NHM dashboard design.
#'
#' @param front UI elements to place on the front of the card.
#' @param back UI elements to place on the back of the card.
#' @param front_title Optional character. Title displayed on the front.
#' @param back_title Optional character. Title displayed on the back.
#' @param tag Optional character. Small uppercase tag text shown on both sides.
#' @param height CSS height for the card. Defaults to \code{"320px"}.
#' @param hint Character or \code{NULL}. Optional hint text shown at the
#'   bottom of both sides. Defaults to \code{NULL} (no hint).
#' @param bg_image Optional character. URL or path to a background image
#'   shown on the front face with a faded overlay.
#' @param bg_opacity Numeric. Opacity of the background image (0 to 1).
#'   Defaults to \code{0.3}.
#'
#' @return A \code{shiny.tag} div element.
#' @export
nhm_flip_card <- function(front,
                          back,
                          front_title = NULL,
                          back_title = NULL,
                          tag = NULL,
                          height = "320px",
                          hint = NULL,
                          bg_image = NULL,
                          bg_opacity = 0.3) {

  tag_el <- if (!is.null(tag) && nzchar(tag)) {
    shiny::tags$span(class = "box-tag", tag)
  }

  hint_el <- if (!is.null(hint) && nzchar(hint)) {
    shiny::tags$div(class = "flip-hint", hint)
  }

  front_title_el <- if (!is.null(front_title) && nzchar(front_title)) {
    shiny::tags$h3(front_title)
  }

  back_title_el <- if (!is.null(back_title) && nzchar(back_title)) {
    shiny::tags$h3(back_title)
  }

  flip_js <- htmltools::htmlDependency(
    name    = "nhm-flip-card",
    version = "0.1.0",
    src     = system.file("www", package = "shinynhm"),
    script  = "js/nhm-flip-card.js"
  )

  shiny::tags$div(
    class = "nhm-flip-card",
    style = paste0("height:", height),
    tabindex = "0",
    role = "button",
    `aria-label` = paste0("Flip card: ", front_title %||% "details"),
    `aria-expanded` = "false",
    flip_js,
    shiny::tags$div(
      class = "nhm-flip-card-inner",
      shiny::tags$div(
        class         = "nhm-flip-card-front",
        if (!is.null(bg_image) && nzchar(bg_image)) {
          shiny::tags$div(
            class             = "card-bg",
            role              = "presentation",
            `aria-hidden`     = "true",
            style = paste0(
              "background-image:url('", htmltools::htmlEscape(bg_image), "');",
              "opacity:", bg_opacity
            )
          )
        },
        tag_el,
        front_title_el,
        front,
        hint_el
      ),
      shiny::tags$div(
        class         = "nhm-flip-card-back",
        `aria-hidden` = "true",
        tag_el,
        back_title_el,
        back,
        hint_el
      )
    )
  )
}
