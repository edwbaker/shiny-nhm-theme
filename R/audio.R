#' Create a themed audio player
#'
#' Renders a custom audio player styled with the NHM theme colours.
#' Use this instead of a raw \code{<audio>} tag for consistent
#' dark-theme styling.
#'
#' @param src Character. The URL or path to the audio file.
#' @param type Character. MIME type of the audio. Defaults to
#'   \code{"audio/wav"}.
#'
#' @return A \code{shiny.tag} div element.
#' @export
nhm_audio <- function(src, type = "audio/wav") {
  js_dep <- htmltools::htmlDependency(
    name    = "nhm-audio",
    version = "0.1.0",
    src     = system.file("www", package = "shinynhm"),
    script  = "js/nhm-audio.js"
  )

  shiny::tags$div(
    class      = "nhm-audio-player",
    `data-src` = src,
    `data-type` = type,
    js_dep,
    shiny::tags$button(
      class        = "nhm-audio-play",
      `aria-label` = "Play",
      shiny::HTML("&#9654;")
    ),
    shiny::tags$span(class = "nhm-audio-time",
      shiny::tags$span(class = "nhm-audio-current", "0:00"),
      " / ",
      shiny::tags$span(class = "nhm-audio-duration", "0:00")
    ),
    shiny::tags$div(class = "nhm-audio-seek",
      shiny::tags$div(class = "nhm-audio-seek-fill"),
      shiny::tags$div(class = "nhm-audio-seek-thumb")
    ),
    shiny::tags$button(
      class        = "nhm-audio-vol-btn",
      `aria-label` = "Mute",
      shiny::HTML("&#128265;")
    ),
    shiny::tags$div(class = "nhm-audio-vol",
      shiny::tags$div(class = "nhm-audio-vol-fill"),
      shiny::tags$div(class = "nhm-audio-vol-thumb")
    ),
    shiny::tags$div(class = "nhm-audio-options",
      shiny::tags$button(
        class        = "nhm-audio-options-btn",
        `aria-label` = "Options",
        `aria-expanded` = "false",
        shiny::HTML("&#8942;")
      ),
      shiny::tags$div(class = "nhm-audio-options-menu",
        shiny::tags$a(class = "nhm-audio-download", href = src, download = "", "Download"),
        shiny::tags$div(class = "nhm-audio-speed",
          shiny::tags$span("Speed"),
          shiny::tags$div(class = "nhm-audio-speed-options",
            shiny::tags$button(class = "nhm-audio-speed-btn", `data-speed` = "0.5", "0.5x"),
            shiny::tags$button(class = "nhm-audio-speed-btn active", `data-speed` = "1", "1x"),
            shiny::tags$button(class = "nhm-audio-speed-btn", `data-speed` = "1.5", "1.5x"),
            shiny::tags$button(class = "nhm-audio-speed-btn", `data-speed` = "2", "2x")
          )
        )
      )
    )
  )
}
