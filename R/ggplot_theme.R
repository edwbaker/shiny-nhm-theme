#' NHM ggplot2 theme
#'
#' A ggplot2 theme matching the NHM dark dashboard style. Works well
#' inside \code{nhm_panel()} in Shiny apps.
#'
#' @param palette Character. Passed to \code{\link{nhm_colours}}.
#' @param base_size Base font size in points.
#' @return A \code{ggplot2::theme} object.
#' @export
theme_nhm <- function(palette = "default", base_size = 14) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for theme_nhm().", call. = FALSE)
  }

  cols <- nhm_colours(palette)

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = cols$card,
                                                  colour = NA),
      panel.background   = ggplot2::element_rect(fill = cols$card,
                                                  colour = NA),
      panel.grid.major   = ggplot2::element_line(colour = cols$border,
                                                  linewidth = 0.4),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(colour = "#ffffff",
                                                  face = "bold"),
      plot.subtitle      = ggplot2::element_text(colour = cols$muted),
      axis.title         = ggplot2::element_text(colour = cols$text),
      axis.text          = ggplot2::element_text(colour = cols$muted),
      legend.background  = ggplot2::element_rect(fill = cols$card,
                                                  colour = NA),
      legend.text        = ggplot2::element_text(colour = cols$text),
      legend.title       = ggplot2::element_text(colour = cols$text),
      strip.text         = ggplot2::element_text(colour = "#ffffff",
                                                  face = "bold")
    )
}

#' NHM discrete colour scale
#'
#' A colour scale using the NHM palette for discrete aesthetics.
#'
#' @param ... Arguments passed to \code{ggplot2::discrete_scale}.
#' @return A ggplot2 colour scale.
#' @export
scale_colour_nhm <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  ggplot2::discrete_scale("colour", "nhm",
                          palette = function(n) nhm_palette(n), ...)
}

#' @rdname scale_colour_nhm
#' @export
scale_color_nhm <- scale_colour_nhm

#' NHM discrete fill scale
#'
#' A fill scale using the NHM palette for discrete aesthetics.
#'
#' @param ... Arguments passed to \code{ggplot2::discrete_scale}.
#' @return A ggplot2 fill scale.
#' @export
scale_fill_nhm <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  ggplot2::discrete_scale("fill", "nhm",
                          palette = function(n) nhm_palette(n), ...)
}

#' Apply NHM theme to a plotly chart
#'
#' Styles a plotly object with NHM dark theme colours: background,
#' axis text, grid lines, legend, and font. Also accepts mapbox
#' configuration for map-based charts.
#'
#' @param p A plotly object.
#' @param palette Character. Passed to \code{\link{nhm_colours}}.
#' @param mapbox A list of mapbox layout options (e.g. style, zoom,
#'   center). If \code{NULL} (default), no mapbox settings are applied.
#' @param show_controls Logical. Whether to show the plotly mode bar.
#'   Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to
#'   \code{plotly::layout()}.
#' @return The styled plotly object.
#' @export
nhm_plotly_layout <- function(p, palette = "default", mapbox = NULL,
                              show_controls = FALSE, ...) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for nhm_plotly_layout().",
         call. = FALSE)
  }

  cols <- nhm_colours(palette)

  axis_style <- list(
    color      = cols$muted,
    gridcolor  = cols$border,
    linecolor  = cols$border,
    zerolinecolor = cols$border
  )

  p <- plotly::layout(p,
    paper_bgcolor = cols$card,
    plot_bgcolor  = cols$card,
    font   = list(color = cols$text),
    xaxis  = axis_style,
    yaxis  = axis_style,
    legend = list(
      bgcolor   = cols$card,
      font      = list(color = cols$text)
    ),
    ...
  )

  if (!is.null(mapbox)) {
    p <- plotly::layout(p, mapbox = mapbox)
  }

  plotly::config(p, displayModeBar = show_controls,
                 scrollZoom = FALSE)
}

#' NHM-themed interactive world map
#'
#' Creates a plotly geo object with an outline world map styled in NHM
#' dark theme colours. Optionally adds markers with hover-enlarge
#' behaviour and click event support.
#'
#' When \code{data} is supplied the function adds a \code{scattergeo}
#' marker trace automatically. Markers enlarge on hover and shrink back
#' on unhover. The plotly click event is registered so that
#' \code{plotly::event_data("plotly_click")} can be used in Shiny to
#' read \code{customdata} from the clicked point.
#'
#' @param palette Character. Passed to \code{\link{nhm_colours}}.
#' @param projection Map projection type. Defaults to
#'   \code{"natural earth"}.
#' @param data Optional data frame of marker locations.
#' @param lat,lon Column names (as formulas, e.g. \code{~lat}) for
#'   latitude and longitude in \code{data}.
#' @param label Column (formula) used as the hover label.
#' @param customdata Column (formula) of HTML strings shown on click.
#' @param marker_size Base marker size in pixels (default 10).
#' @param hover_size Marker size on hover (default 18).
#' @param marker_colour Marker fill colour. Used when
#'   \code{marker_values} is \code{NULL}. Defaults to the palette lime
#'   colour.
#' @param marker_values Optional numeric vector (or formula) of values
#'   used to colour markers on a continuous scale via
#'   \code{\link{nhm_colour_ramp}}. When supplied, \code{marker_colour}
#'   is ignored.
#' @param colour_limits Length-2 numeric vector giving the value range
#'   for the colour ramp. Defaults to the range of \code{marker_values}.
#' @param ramp_colours Character vector of colours for the continuous
#'   ramp. Passed to \code{\link{nhm_colour_ramp}}.
#' @param show_controls Logical. Whether to show the plotly mode bar.
#'   Defaults to \code{FALSE}.
#' @param allow_zoom Logical. Whether to allow scroll-wheel zooming.
#'   Defaults to \code{FALSE}.
#' @param show_colorbar Logical. Whether to display a colour scale
#'   legend alongside the map. Only used when \code{marker_values} is
#'   supplied. Defaults to \code{FALSE}.
#' @param colorbar_title Title text for the colour bar. Defaults to
#'   \code{""}.
#' @param ... Additional arguments passed to \code{plotly::layout()}.
#' @return A plotly object with the styled base map (and markers if
#'   \code{data} was provided).
#' @export
nhm_world_map <- function(palette = "default",
                          projection = "natural earth",
                          data = NULL,
                          lat = NULL, lon = NULL,
                          label = NULL, customdata = NULL,
                          marker_size = 10, hover_size = 18,
                          marker_colour = NULL,
                          marker_values = NULL,
                          colour_limits = NULL,
                          ramp_colours = NULL,
                          show_controls = FALSE,
                          allow_zoom = FALSE,
                          show_colorbar = FALSE,
                          colorbar_title = "", ...) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for nhm_world_map().",
         call. = FALSE)
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required for nhm_world_map().",
         call. = FALSE)
  }

  cols <- nhm_colours(palette)
  if (is.null(marker_colour)) marker_colour <- cols$lime

  geo_style <- list(
    showframe       = FALSE,
    showcoastlines  = TRUE,
    coastlinecolor  = cols$cyan,
    coastlinewidth  = 0.8,
    showland        = FALSE,
    showocean       = FALSE,
    showlakes       = FALSE,
    showcountries   = TRUE,
    countrycolor    = cols$cyan,
    countrywidth    = 0.4,
    bgcolor         = cols$card,
    projection      = list(type = projection)
  )

  p <- plotly::plot_ly()

  drag <- if (allow_zoom) "zoom" else FALSE

  p <- plotly::layout(p,
    geo           = geo_style,
    paper_bgcolor = cols$card,
    plot_bgcolor  = cols$card,
    font          = list(color = cols$text),
    hoverlabel    = list(
      bgcolor     = cols$card,
      font        = list(color = cols$text, size = 13),
      bordercolor = cols$cyan
    ),
    dragmode      = drag,
    margin        = list(l = 0, r = 0, t = 30, b = 0),
    ...
  )

  if (!is.null(data)) {
    # Resolve marker colours
    if (!is.null(marker_values)) {
      # Evaluate formula if needed
      vals <- if (inherits(marker_values, "formula")) {
        eval(marker_values[[2]], data)
      } else {
        marker_values
      }
      resolved_colours <- nhm_colour_ramp(vals, colours = ramp_colours,
                                          limits = colour_limits,
                                          palette = palette)
    } else {
      resolved_colours <- marker_colour
    }

    # Border colour for hoverlabel: use first ramp colour or marker_colour
    border_col <- if (!is.null(marker_values)) {
      nhm_colours(palette)$cyan
    } else {
      marker_colour
    }
    p <- plotly::layout(p,
      hoverlabel = list(
        bgcolor     = cols$card,
        font        = list(color = cols$text, size = 13),
        bordercolor = border_col
      )
    )

    trace_args <- list(
      p          = p,
      type       = "scattergeo",
      mode       = "markers",
      data       = data,
      lat        = lat,
      lon        = lon,
      marker     = list(
        size     = marker_size,
        sizemode = "diameter",
        color    = resolved_colours,
        opacity  = 0.9,
        line     = list(color = resolved_colours, width = 0.5)
      ),
      hoverinfo  = "text"
    )

    if (!is.null(label)) {
      trace_args$hovertemplate <- paste0(
        "<b>%{text}</b><extra></extra>"
      )
      trace_args$text <- label
    }

    if (!is.null(customdata)) {
      trace_args$customdata <- customdata
    }

    p <- do.call(plotly::add_trace, trace_args)

    # Add a colour bar via a dummy trace with the matching colorscale
    if (show_colorbar && !is.null(marker_values)) {
      vals <- if (inherits(marker_values, "formula")) {
        eval(marker_values[[2]], data)
      } else {
        marker_values
      }
      clims <- if (!is.null(colour_limits)) colour_limits else range(vals, na.rm = TRUE)

      # Build the plotly colorscale from ramp colours
      ramp_cols <- if (!is.null(ramp_colours)) ramp_colours else {
        c(nhm_colours(palette)$cyan, nhm_colours(palette)$lime,
          nhm_colours(palette)$pink)
      }
      n_ramp <- length(ramp_cols)
      colorscale <- lapply(seq_along(ramp_cols), function(i) {
        list((i - 1) / (n_ramp - 1), ramp_cols[i])
      })

      p <- plotly::add_trace(p,
        type   = "scattergeo",
        mode   = "markers",
        lat    = vals,
        lon    = vals,
        marker = list(
          size     = 0,
          opacity  = 0,
          color    = vals,
          cmin     = clims[1],
          cmax     = clims[2],
          colorscale = colorscale,
          showscale  = TRUE,
          colorbar   = list(
            title       = list(text = colorbar_title,
                               font = list(color = cols$text, size = 12)),
            tickfont    = list(color = cols$muted, size = 11),
            bgcolor     = "rgba(0,0,0,0)",
            borderwidth = 0,
            len         = 0.6,
            thickness   = 15,
            x           = 1.02,
            y           = 0.5
          )
        ),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    }
  }

  p <- plotly::event_register(p, "plotly_click")

  # Enlarge markers on hover
  js <- sprintf("
    function(el) {
      var baseSize = %d;
      var hoverSize = %d;
      el.on('plotly_hover', function(d) {
        var idx = d.points[0].curveNumber;
        var sizes = el.data[idx].marker.size;
        if (typeof sizes === 'number') {
          sizes = Array(el.data[idx].lat.length).fill(baseSize);
        } else {
          sizes = sizes.slice();
        }
        sizes[d.points[0].pointIndex] = hoverSize;
        var update = {};
        update['marker.size'] = [sizes];
        Plotly.restyle(el, update, [idx]);
      });
      el.on('plotly_unhover', function(d) {
        var idx = d.points[0].curveNumber;
        var sizes = Array(el.data[idx].lat.length).fill(baseSize);
        var update = {};
        update['marker.size'] = [sizes];
        Plotly.restyle(el, update, [idx]);
      });
    }
  ", marker_size, hover_size)
  p <- htmlwidgets::onRender(p, js)

  plotly::config(p, displayModeBar = show_controls,
                 scrollZoom = allow_zoom)
}
