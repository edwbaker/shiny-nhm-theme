#' Create an NHM-styled header bar
#'
#' Generates a header containing the NHM logo and configurable title text.
#'
#' @param title Character. The main title displayed in the header.
#' @param subbrand Character. Smaller text displayed above the title
#'   (e.g. "NATURAL HISTORY MUSEUM X AWS"). Set to \code{NULL} to hide.
#' @param description Character. Small muted text displayed below the title.
#'   Set to \code{NULL} to hide.
#' @param logo_height Character. CSS height for the logo image.
#'   Defaults to \code{"55px"}.
#' @param right_content Optional. Additional Shiny UI elements to place
#'   on the right side of the header.
#'
#' @return A \code{shiny.tag} div element.
#' @export
nhm_header <- function(title = "Dashboard",
                       subbrand = "NATURAL HISTORY MUSEUM",
                       description = NULL,
                       logo_height = "55px",
                       right_content = NULL) {
  logo_src <- "shinynhm/images/nhm_logo_white.png"

  subbrand_tag <- if (!is.null(subbrand) && nzchar(subbrand)) {
    shiny::tags$p(class = "nhm-header-subbrand", subbrand)
  }

  description_tag <- if (!is.null(description) && nzchar(description)) {
    shiny::tags$p(class = "nhm-header-description", description)
  }

  shiny::tags$div(
    class = "nhm-header",
    role  = "banner",
    shiny::tags$img(
      src   = logo_src,
      alt   = "Natural History Museum",
      class = "nhm-header-logo",
      style = paste0("height:", logo_height)
    ),
    shiny::tags$div(
      class = "nhm-header-text",
      subbrand_tag,
      shiny::tags$h1(class = "nhm-header-title", title),
      description_tag
    ),
    if (!is.null(right_content)) {
      shiny::tags$div(class = "nhm-header-right", right_content)
    }
  )
}


#' Create a full NHM-themed page
#'
#' Wraps content in a page that includes the NHM theme, a header with
#' configurable title, and an optional footer.
#'
#' @param ... UI elements to place in the page body.
#' @param title Character. The main title for the header.
#' @param subbrand Character. Smaller text above the title.
#' @param description Character. Small muted text below the title.
#' @param window_title Character. The browser tab title. Defaults to
#'   the value of \code{title}.
#' @param footer Logical or character. If \code{TRUE}, shows the default
#'   NHM copyright footer. If a character string, uses that as the footer
#'   text. If \code{FALSE} or \code{NULL}, no footer is shown.
#' @param right_header Optional. Additional UI to place on the right of
#'   the header bar.
#' @param logo_height Character. CSS height for the logo.
#'
#' @return A Shiny UI definition.
#' @export
nhm_page <- function(...,
                     title = "Dashboard",
                     subbrand = "NATURAL HISTORY MUSEUM",
                     description = NULL,
                     window_title = title,
                     footer = TRUE,
                     right_header = NULL,
                     logo_height = "55px",
                     palette = "default") {
  palette <- match.arg(palette, c("default", "nhm-web"))

  palette_style <- if (palette != "default") {
    cols <- nhm_colours(palette)
    shiny::tags$style(shiny::HTML(sprintf(
      "html:root {
  --nhm-deep: %s !important;
  --nhm-purple: %s !important;
  --nhm-card: %s !important;
  --nhm-card-hover: %s !important;
  --nhm-cyan: %s !important;
  --nhm-lime: %s !important;
  --nhm-pink: %s !important;
  --nhm-blue: %s !important;
  --nhm-brown: %s !important;
  --nhm-text: %s !important;
  --nhm-muted: %s !important;
  --nhm-border: %s !important;
  --nhm-input: %s !important;
  --nhm-success: %s !important;
  --nhm-danger: %s !important;
  --nhm-warning: %s !important;
}",
      cols$deep, cols$purple, cols$card, cols$card_hover,
      cols$cyan, cols$lime, cols$pink, cols$blue, cols$brown,
      cols$text, cols$muted, cols$border, cols$input,
      cols$success, cols$danger, cols$warning
    )))
  }

  footer_tag <- if (is.character(footer) && nzchar(footer)) {
    shiny::tags$footer(class = "nhm-footer", role = "contentinfo", footer)
  } else if (isTRUE(footer)) {
    current_year <- format(Sys.Date(), "%Y")
    shiny::tags$footer(
      class = "nhm-footer",
      role  = "contentinfo",
      shiny::HTML(paste0(
        "&copy; ", current_year,
        " The Trustees of the Natural History Museum, London"
      ))
    )
  }

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$title(window_title),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$script(shiny::HTML("document.documentElement.lang='en';"))
    ),
    shiny::tags$a(
      class = "nhm-skip-link",
      href  = "#nhm-main-content",
      "Skip to main content"
    ),
    nhm_theme(),
    palette_style,
    nhm_header(
      title         = title,
      subbrand      = subbrand,
      description   = description,
      logo_height   = logo_height,
      right_content = right_header
    ),
    shiny::tags$main(
      id   = "nhm-main-content",
      role = "main",
      shiny::fluidPage(...)
    ),
    footer_tag
  )
}


#' Create an NHM-styled panel
#'
#' A styled container for grouping related content.
#'
#' @param ... UI elements to place inside the panel.
#' @param title Optional character. If provided, rendered as a heading
#'   at the top of the panel.
#' @param heading_level Integer. HTML heading level for the title
#'   (2 = \code{h2}, 3 = \code{h3}, etc.). Defaults to \code{3}.
#' @param bg_image Optional character. URL or path to a background image.
#'   The image is shown with a faded overlay.
#' @param bg_opacity Numeric. Opacity of the background image (0 to 1).
#'   Defaults to \code{0.3}.
#'
#' @return A \code{shiny.tag} div element.
#' @export
nhm_panel <- function(..., title = NULL, heading_level = 3L,
                      bg_image = NULL, bg_opacity = 0.3) {
  title_tag <- if (!is.null(title)) {
    heading_fn <- switch(as.character(heading_level),
      "2" = shiny::tags$h2,
      "3" = shiny::tags$h3,
      "4" = shiny::tags$h4,
      "5" = shiny::tags$h5,
      shiny::tags$h3
    )
    heading_fn(title)
  }

  bg_tag <- if (!is.null(bg_image) && nzchar(bg_image)) {
    shiny::tags$div(
      class = "card-bg",
      style = paste0(
        "background-image:url('", htmltools::htmlEscape(bg_image), "');",
        "opacity:", bg_opacity
      )
    )
  }

  shiny::tags$div(
    class = "nhm-card",
    bg_tag,
    title_tag,
    ...
  )
}
