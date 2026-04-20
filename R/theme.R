#' Add the NHM theme CSS to a Shiny app
#'
#' Includes the NHM dark theme stylesheet and any required assets.
#' Place this in the UI, typically inside \code{fluidPage()} or
#' \code{navbarPage()}.
#'
#' @return An HTML dependency that loads the NHM theme CSS.
#' @export
nhm_theme <- function() {
  htmltools::htmlDependency(
    name    = "nhm-theme",
    version = "0.1.0",
    src     = system.file("www", package = "shinynhm"),
    stylesheet = "css/nhm-theme.css",
    all_files  = TRUE
  )
}
