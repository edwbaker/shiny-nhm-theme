#' Apply the NHM theme to base R plots
#'
#' Sets \code{par()} parameters to match the NHM dark theme. Call this
#' inside \code{renderPlot()} before any plotting code.
#'
#' @param bg Background colour. Defaults to the card colour.
#' @param fg Foreground colour (axes, box). Defaults to muted text.
#' @param col Default plotting colour (lines, points, bars). Defaults
#'   to the first palette colour (cyan).
#' @param col.axis Axis tick-label colour. Defaults to theme text.
#' @param col.lab Axis-label colour. Defaults to theme text.
#' @param col.main Title colour. Defaults to white.
#' @param mar Plot margins as \code{c(bottom, left, top, right)}.
#' @param ... Additional arguments passed to \code{par()}.
#'
#' @return The previous \code{par()} settings (invisibly), so they can
#'   be restored with \code{par(old)}.
#' @importFrom graphics par
#' @export
nhm_par <- function(bg       = NULL,
                    fg       = NULL,
                    col      = NULL,
                    col.axis = NULL,
                    col.lab  = NULL,
                    col.main = NULL,
                    mar      = c(4, 4, 2, 1),
                    ...) {
  cols <- nhm_colours()

  old <- par(
    bg       = bg       %||% cols$card,
    fg       = fg       %||% cols$muted,
    col      = col      %||% cols$cyan,
    col.axis = col.axis %||% cols$text,
    col.lab  = col.lab  %||% cols$text,
    col.main = col.main %||% "#ffffff",
    col.sub  = cols$muted,
    mar      = mar,
    family   = "",
    ...
  )
  invisible(old)
}

#' NHM spectrogram palette
#'
#' Returns a palette function suitable for the \code{palette} argument of
#' \code{seewave::spectro()}. The palette ramps from the NHM card
#' background colour through cyan to lime, so silence blends into the
#' panel and loud signals stand out.
#'
#' @param colours Character vector of colours to interpolate. Defaults
#'   to a ramp from the card background through cyan to lime.
#' @return A function that accepts an integer \code{n} and returns
#'   \code{n} interpolated hex colours.
#' @importFrom grDevices colorRampPalette
#' @export
nhm_spectro_palette <- function(colours = NULL) {
  cols <- nhm_colours()
  if (is.null(colours)) {
    colours <- c(cols$card, cols$purple, cols$cyan, cols$lime)
  }
  colorRampPalette(colours)
}

#' NHM colour palette for data series
#'
#' Returns a character vector of colours suitable for distinguishing
#' data series in plots. The palette is ordered for maximum contrast
#' against the dark NHM background.
#'
#' @param n Number of colours to return. If \code{NULL} (default),
#'   returns the full palette.
#' @return A character vector of hex colour strings.
#' @export
nhm_palette <- function(n = NULL) {
  cols <- nhm_colours()
  pal <- c(cols$cyan, cols$lime, cols$pink, cols$blue,
           cols$brown, cols$success, cols$danger, cols$warning)
  if (!is.null(n)) pal <- rep_len(pal, n)
  pal
}

#' Themed grid for base R plots
#'
#' Draws a grid using the NHM border colour. Designed to be used as the
#' \code{panel.first} argument of \code{plot()}.
#'
#' @param nx,ny Number of cells in the x and y directions (passed to
#'   \code{\link[graphics]{grid}}).
#' @param col Grid line colour. Defaults to the NHM border colour.
#' @param lty Grid line type.
#' @return Called for its side-effect; returns \code{NULL} invisibly.
#' @importFrom graphics grid
#' @export
nhm_grid <- function(nx = NULL, ny = nx, col = NULL, lty = 1) {
  cols <- nhm_colours()
  grid(nx = nx, ny = ny, col = col %||% cols$border, lty = lty)
}

#' NHM-themed legend
#'
#' A convenience wrapper around \code{\link[graphics]{legend}} pre-filled
#' with NHM dark-theme colours.
#'
#' @param x,y Position (see \code{\link[graphics]{legend}}).
#' @param legend Character vector of legend labels.
#' @param col Colours for legend symbols (typically from \code{nhm_palette()}).
#' @param ... Additional arguments passed to \code{legend()}.
#' @return The value returned by \code{legend()}, invisibly.
#' @importFrom graphics legend
#' @export
nhm_legend <- function(x, y = NULL, legend, col, ...) {
  cols <- nhm_colours()
  invisible(
    legend(x, y, legend = legend, col = col,
           text.col = cols$text, bg = cols$purple,
           box.col = cols$border, ...)
  )
}
