#' NHM colour palette
#'
#' Returns the NHM theme colour palette as a named list.
#'
#' @param palette Character. Which palette to return. \code{"default"} is
#'   the original dashboard theme. \code{"nhm-web"} matches the colours
#'   used on nhm.ac.uk.
#' @return A named list of hex colour strings.
#' @export
nhm_colours <- function(palette = "default") {
  palette <- match.arg(palette, c("default", "nhm-web"))

  if (palette == "nhm-web") {
    return(list(
      deep       = "#290340",
      purple     = "#3e1c53",
      card       = "#000040",
      card_hover = "#523364",
      cyan       = "#C9F708",
      lime       = "#C9F708",
      pink       = "#F2BAB0",
      blue       = "#0d17f5",
      brown      = "#8F7E76",
      text       = "#ffffff",
      muted      = "#a99ab3",
      border     = "#523364",
      input      = "#523364",
      success    = "#C9F708",
      danger     = "#e53f3d",
      warning    = "#fed200"
    ))
  }

  list(
    deep       = "#290340",
    purple     = "#3a0a55",
    card       = "#35104a",
    card_hover = "#4a2668",
    cyan       = "#0AEDF6",
    lime       = "#C8E64A",
    pink       = "#F0A0D0",
    blue       = "#0E17F8",
    brown      = "#8F7E76",
    text       = "#ede8f0",
    muted      = "#a89bb3",
    border     = "#4a2668",
    input      = "#4a2668",
    success    = "#C8E64A",
    danger     = "#e74c3c",
    warning    = "#f39c12"
  )
}
