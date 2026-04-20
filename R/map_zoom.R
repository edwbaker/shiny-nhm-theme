#' Fly-to animation for plotly geo maps
#'
#' Smoothly animates a plotly scattergeo map from its current view to a
#' target location by interpolating the projection centre and scale.
#' Requires the map output to include \code{\link{nhm_map_zoom_js}()}
#' so the JavaScript handler is registered.
#'
#' @param session The Shiny session object.
#' @param outputId The output ID of the \code{plotlyOutput} to animate.
#' @param lat Target latitude.
#' @param lon Target longitude.
#' @param zoom Projection scale (1 = whole globe, higher = more zoomed).
#'   Typical values: 1 for global, 3 for continent, 6 for country,
#'   15\u201325 for city.
#' @param duration Animation duration in milliseconds. Default 1500.
#' @param geo_features Optional named list of geo layout properties to
#'   apply at the destination (e.g.
#'   \code{list(showrivers = TRUE, showlakes = TRUE)}).
#'
#' @param mapbox_layers Optional list of mapbox layer definitions
#'   to apply before the animation starts. Used for swapping GeoJSON
#'   boundary layers at different zoom levels.
#'
#' @return Called for its side effect (sends a message to the client).
#' @export
nhm_map_flyto <- function(session, outputId, lat, lon,
                          zoom = 1, duration = 1500,
                          geo_features = NULL,
                          mapbox_layers = NULL) {
  msg <- list(
    outputId     = outputId,
    lat          = lat,
    lon          = lon,
    zoom         = zoom,
    duration     = duration,
    geo_features = geo_features
  )
  if (!is.null(mapbox_layers)) msg$mapbox_layers <- mapbox_layers
  session$sendCustomMessage("nhm-map-flyto", msg)
}

#' Reset a plotly geo map to the global view
#'
#' Flies back to the default global view (lat 0, lon 0, scale 1).
#'
#' @inheritParams nhm_map_flyto
#' @param duration Animation duration in milliseconds. Default 1200.
#'
#' @return Called for its side effect.
#' @export
nhm_map_reset <- function(session, outputId, duration = 1200) {
  nhm_map_flyto(session, outputId,
                lat = 0, lon = 0, zoom = 1,
                duration = duration,
                geo_features = list(
                  showrivers   = FALSE,
                  showlakes    = FALSE,
                  showsubunits = FALSE
                ))
}

#' Multi-stage fly-through animation
#'
#' Automatically generates intermediate zoom stages between the current
#' globe view and a target location, spacing zoom levels logarithmically
#' and enabling map features (countries, sub-units, rivers, lakes)
#' progressively as the zoom increases.
#'
#' @param session The Shiny session object.
#' @param outputId The output ID of the \code{plotlyOutput}.
#' @param lat Target latitude.
#' @param lon Target longitude.
#' @param zoom Final projection scale. Higher values = more zoomed in.
#'   Typical values: 3 continent, 8 country, 25 city, 80\u2013150
#'   building/street level.
#' @param stages Number of intermediate zoom stages (default 5).
#'   More stages = smoother but slower journey.
#' @param duration_per_stage Milliseconds per stage. Default 1200.
#' @param pause_between Milliseconds pause between stages. Default 200.
#' @param start_lat,start_lon Starting position (default 0, 0 = globe
#'   centre).
#' @param start_zoom Starting zoom level (default 1 = globe).
#' @param palette Colour palette for sub-unit borders.
#' @param mapbox_layers A named list of GeoJSON layer definitions
#'   keyed by minimum zoom level. Each entry is a list of mapbox
#'   layer specs (with \code{sourcetype}, \code{source}, \code{type},
#'   etc.). Layers are swapped in as zoom increases. Ignored for
#'   scattergeo maps.
#' @param on_complete Optional function called when animation finishes.
#'
#' @return Called for its side effect. Returns (invisibly) the total
#'   animation duration in seconds.
#' @export
nhm_map_flythrough <- function(session, outputId,
                               lat, lon, zoom = 80,
                               stages = 5,
                               duration_per_stage = 1200,
                               pause_between = 200,
                               start_lat = 0, start_lon = 0,
                               start_zoom = 1,
                               palette = "default",
                               mapbox_layers = NULL,
                               on_complete = NULL) {
  cols <- nhm_colours(palette)

  # Generate logarithmically-spaced zoom levels
  log_start <- log(start_zoom)
  log_end   <- log(zoom)
  log_zooms <- seq(log_start, log_end, length.out = stages + 1)[-1]
  zoom_seq  <- exp(log_zooms)

  # Centre on target from the start — at low zoom the whole globe is

  # visible so there's no visual jump, and every stage keeps the
  # target in view.
  lat_seq <- rep(lat, stages)
  lon_seq <- rep(lon, stages)

  # Determine geo features based on zoom level
  features_for_zoom <- function(z) {
    f <- list()
    if (z >= 2)  f$showcountries <- TRUE
    if (z >= 2)  f$countrywidth  <- min(0.4 + z * 0.05, 1.2)
    if (z >= 6)  f$showsubunits  <- TRUE
    if (z >= 6)  f$subunitwidth  <- 0.5
    if (z >= 6)  f$subunitcolor  <- cols$lime
    if (z >= 10) f$showrivers    <- TRUE
    if (z >= 10) f$rivercolor    <- "rgba(0,200,255,0.3)"
    if (z >= 10) f$riverwidth    <- min(0.5 + z * 0.02, 2)
    if (z >= 20) f$showlakes     <- TRUE
    if (z >= 20) f$lakecolor     <- "rgba(0,200,255,0.15)"
    if (z >= 20) f$rivercolor    <- "rgba(0,200,255,0.4)"
    if (z >= 20) f$riverwidth    <- min(1 + z * 0.015, 2.5)
    f
  }

  # Determine mapbox layers for a given zoom level
  layers_for_zoom <- function(z) {
    if (is.null(mapbox_layers)) return(NULL)
    # Find the highest threshold <= current zoom and use that set
    thresholds <- as.numeric(names(mapbox_layers))
    applicable <- thresholds[thresholds <= z]
    if (length(applicable) == 0) return(NULL)
    best <- max(applicable)
    mapbox_layers[[as.character(best)]]
  }

  # Schedule each stage
  step_time <- duration_per_stage + pause_between
  for (i in seq_len(stages)) {
    local({
      ii <- i
      delay_s <- (ii - 1) * step_time / 1000
      later::later(function() {
        nhm_map_flyto(
          session       = session,
          outputId      = outputId,
          lat           = lat_seq[ii],
          lon           = lon_seq[ii],
          zoom          = zoom_seq[ii],
          duration      = duration_per_stage,
          geo_features  = features_for_zoom(zoom_seq[ii]),
          mapbox_layers = layers_for_zoom(zoom_seq[ii])
        )
      }, delay = delay_s)
    })
  }

  total_s <- (stages - 1) * step_time / 1000 + duration_per_stage / 1000

  # Call completion callback
  if (is.function(on_complete)) {
    later::later(on_complete, delay = total_s + 0.3)
  }

  invisible(total_s)
}

#' JavaScript dependency for map fly-to animations
#'
#' Returns a \code{shiny::tags$script} element that registers the
#' \code{nhm-map-flyto} custom Shiny message handler. Include this once
#' in your UI (e.g. inside \code{nhm_page()}).
#'
#' @return An HTML script tag.
#' @export
nhm_map_zoom_js <- function() {
  shiny::tags$script(shiny::HTML("
    (function() {
      var _nhmFlyAnimId = null;
      var _nhmGeoFeatures = {};

      Shiny.addCustomMessageHandler('nhm-map-flyto', function(msg) {
        if (_nhmFlyAnimId) {
          cancelAnimationFrame(_nhmFlyAnimId);
          _nhmFlyAnimId = null;
        }

        var el = document.getElementById(msg.outputId);
        if (el && !el._fullLayout) {
          var inner = el.querySelector('.js-plotly-plot') ||
                      el.querySelector('.plotly') ||
                      el.querySelector('.plot-container');
          if (inner && inner._fullLayout) el = inner;
        }
        if (!el || !el._fullLayout) return;

        var layout = el._fullLayout;
        var isMapbox = !!(layout.mapbox);

        var startLat, startLon, startZoom;
        var endLat   = msg.lat;
        var endLon   = msg.lon;
        var endZoom  = msg.zoom;
        var duration = msg.duration || 1500;

        if (isMapbox) {
          // Mapbox-style map (scattermapbox)
          var mb = layout.mapbox;
          startLat  = (mb.center && mb.center.lat) || 0;
          startLon  = (mb.center && mb.center.lon) || 0;
          startZoom = (mb.zoom != null) ? mb.zoom : 1;

          // Apply mapbox layers if provided
          if (msg.mapbox_layers) {
            Plotly.relayout(el, {'mapbox.layers': msg.mapbox_layers});
          }

          // Use native mapbox-gl easeTo for smooth zoom
          var subplot = mb._subplot;
          var map = subplot && subplot.map;
          console.log('[nhm-flyto] subplot:', !!subplot, 'map from subplot:', !!map);
          if (!map) {
            // Fallback: search for mapbox-gl map instance in DOM
            var mapDiv = el.querySelector('.mapboxgl-map');
            console.log('[nhm-flyto] mapDiv:', !!mapDiv, 'mapDiv._mapboxMap:', !!(mapDiv && mapDiv._mapboxMap));
            if (mapDiv && mapDiv._mapboxMap) map = mapDiv._mapboxMap;
          }
          if (!map) {
            // Try plotly's internal reference
            var subplots = el._fullLayout._plots;
            console.log('[nhm-flyto] _plots keys:', subplots ? Object.keys(subplots) : 'none');
            if (subplots) {
              var keys = Object.keys(subplots);
              for (var k = 0; k < keys.length; k++) {
                var sp = subplots[keys[k]];
                if (sp && sp.map) { map = sp.map; break; }
              }
            }
          }
          if (!map && el._fullLayout.mapbox && el._fullLayout.mapbox._subplot) {
            map = el._fullLayout.mapbox._subplot.map;
            console.log('[nhm-flyto] from _fullLayout.mapbox._subplot:', !!map);
          }
          console.log('[nhm-flyto] final map found:', !!map);
          if (map) {
            console.log('[nhm-flyto] using easeTo to', endLat, endLon, 'zoom', endZoom);
            var panDuration = Math.round(duration * 0.4);
            var zoomDuration = Math.round(duration * 0.6);
            // Step 1: pan to target at current zoom
            map.easeTo({
              center:   [endLon, endLat],
              duration: panDuration,
              easing: function(t) { return t < 0.5 ? 2*t*t : -1+(4-2*t)*t; }
            });
            // Step 2: zoom to target level
            map.once('moveend', function() {
              map.easeTo({
                zoom:     endZoom,
                duration: zoomDuration,
                easing: function(t) { return t < 0.5 ? 2*t*t : -1+(4-2*t)*t; }
              });
              map.once('moveend', function() {
                Plotly.relayout(el, {
                  'mapbox.center.lat': endLat,
                  'mapbox.center.lon': endLon,
                  'mapbox.zoom': endZoom
                });
              });
            });
            return;
          }
          console.log('[nhm-flyto] FALLBACK: no mapbox-gl map found, using requestAnimationFrame loop');
        } else {
          // Geo-style map (scattergeo)
          var geo = layout.geo || {};
          var proj = (geo.projection || {});
          startLat  = (proj.rotation && proj.rotation.lat) || 0;
          startLon  = (proj.rotation && proj.rotation.lon) || 0;
          startZoom = proj.scale || 1;

          // Accumulate geo features
          var features = msg.geo_features || {};
          for (var key in features) {
            _nhmGeoFeatures['geo.' + key] = features[key];
          }
          if (endZoom <= 1) {
            _nhmGeoFeatures = {};
            for (var key in features) {
              _nhmGeoFeatures['geo.' + key] = features[key];
            }
          }
        }

        var geoFeats = isMapbox ? {} : Object.assign({}, _nhmGeoFeatures);
        var startTime = null;

        function easeInOutCubic(t) {
          return t < 0.5 ? 4*t*t*t : 1 - Math.pow(-2*t + 2, 3) / 2;
        }

        function step(timestamp) {
          if (!startTime) startTime = timestamp;
          var elapsed = timestamp - startTime;
          var t = Math.min(elapsed / duration, 1);
          var e = easeInOutCubic(t);

          var curLat  = startLat  + (endLat  - startLat)  * e;
          var curLon  = startLon  + (endLon  - startLon)  * e;
          var curZoom = startZoom + (endZoom - startZoom) * e;

          var update;
          if (isMapbox) {
            update = {
              'mapbox.center.lat': curLat,
              'mapbox.center.lon': curLon,
              'mapbox.zoom': curZoom
            };
          } else {
            update = Object.assign({}, geoFeats, {
              'geo.projection.rotation.lat': curLat,
              'geo.projection.rotation.lon': curLon,
              'geo.projection.scale': curZoom,
              'geo.center.lat': curLat,
              'geo.center.lon': curLon
            });
          }

          Plotly.relayout(el, update);

          if (t < 1) {
            _nhmFlyAnimId = requestAnimationFrame(step);
          } else {
            _nhmFlyAnimId = null;
          }
        }

        _nhmFlyAnimId = requestAnimationFrame(step);
      });
    })();
  "))
}
