# Demo: World Cities Heat — animated timeline
# Run with: shiny::runApp("inst/demo-heat")

library(shiny)
library(shinynhm)
library(plotly)

palette <- "default"
cols    <- nhm_colours(palette)

# ── Load data ───────────────────────────────────────────────────
heat <- read.csv(
  system.file("demo-heat/data/heat_cities.csv", package = "shinynhm"),
  stringsAsFactors = FALSE
)

years <- sort(unique(heat$year))

# ── Precompute baseline (first year) change ─────────────────────
baseline <- heat[heat$year == min(years), c("city_name", "hottest_3mo_tasmax_c")]
names(baseline)[2] <- "baseline_temp"
heat <- merge(heat, baseline, by = "city_name", all.x = TRUE)
heat$temp_change <- heat$hottest_3mo_tasmax_c - heat$baseline_temp

# ── Load Met Office station data ─────────────────────────────────
stations <- read.csv(
  system.file("demo-heat/data/met_office_stations.csv", package = "shinynhm"),
  stringsAsFactors = FALSE
)

# ── Load NHM sensor data ─────────────────────────────────────────
sensors <- read.csv(
  system.file("demo-heat/data/nhm_sensors.csv", package = "shinynhm"),
  stringsAsFactors = FALSE
)

# ── Load WMO station data ─────────────────────────────────────────
wmo_stations <- read.csv(
  system.file("demo-heat/data/wmo_stations.csv", package = "shinynhm"),
  stringsAsFactors = FALSE
)

# ── Load GeoJSON boundary data for outline map ──────────────────
# GeoJSON files served as static assets — mapbox-gl loads them by URL
shiny::addResourcePath("geodata",
  system.file("demo-heat/data", package = "shinynhm")
)

# ── Mean distance between devices (from bounding box area / n) ───
bbox_mean_dist <- function(lat, lon) {
  R <- 6371
  lat_r <- range(lat) * pi / 180
  lon_r <- range(lon) * pi / 180
  # Spherical rectangle area
  area_km2 <- R^2 * abs(sin(lat_r[2]) - sin(lat_r[1])) *
    abs(lon_r[2] - lon_r[1])
  sqrt(area_km2 / length(lat))
}

dist_world <- round(bbox_mean_dist(wmo_stations$lat, wmo_stations$lon), 1)
dist_uk    <- round(bbox_mean_dist(stations$lat, stations$lon), 1)
dist_nhm   <- round(bbox_mean_dist(sensors$lat, sensors$lon) * 1000) # metres

# ── UI ──────────────────────────────────────────────────────────
ui <- nhm_page(
  title       = "World Cities Heat Projections",
  subbrand    = "NATURAL HISTORY MUSEUM",
  description = paste0(
    "Projected hottest 3-month average daily maximum temperature (",
    min(years), "\u2013", max(years), ", RCP 8.5)"
  ),
  footer  = FALSE,
  palette = palette,

  nhm_map_zoom_js(),

  nhm_flipbook(
    id = "demo",

    # ── Page 1: Heat map timeline ─────────────────────────────
    nhm_flipbook_page(
      title = "Global Heat Map",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            shiny::radioButtons(
              inputId  = "mode",
              label    = "Display",
              choices  = c("Temperature" = "absolute",
                           "Predicted change" = "change"),
              selected = "absolute",
              inline   = TRUE
            ),
            shiny::hr(),
            nhm_timeline_input(
              inputId  = "year",
              label    = "Year",
              values   = years,
              selected = min(years),
              interval = 600,
              palette  = palette
            ),
            shiny::hr(),
            shiny::tags$p(
              class = "nhm-value-label", "SELECTED YEAR"
            ),
            shiny::tags$p(
              style = paste0(
                "font-size:2.5rem; font-weight:700; color:",
                cols$cyan, "; margin:4px 0;"
              ),
              shiny::textOutput("year_display", inline = TRUE)
            )
          ),
          nhm_panel(
            title = "Site Details",
            shiny::uiOutput("city_detail"),
            plotly::plotlyOutput("city_timeseries", height = "200px")
          ),
          nhm_panel(
            title = "Stats",
            shiny::uiOutput("year_stats")
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Projected Peak Temperature by City",
            plotly::plotlyOutput("heat_map", height = "650px")
          )
        )
      )
    ),

    # ── Page 2: Fly to London ─────────────────────────────────
    nhm_flipbook_page(
      title = "Fly to NHM",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Fly-to Demo",
            shiny::tags$p(
              style = paste0("color:", cols$muted, ";"),
              "Zoom between three views: the whole world, the United",
              " Kingdom, and the Natural History Museum in London."
            ),
            shiny::hr(),
            shiny::actionButton(
              "fly_world", "World",
              icon  = shiny::icon("globe"),
              class = "nhm-btn",
              style = paste0(
                "background:", cols$cyan, ";color:", cols$deep, ";",
                "border:none;border-radius:4px;padding:10px 20px;",
                "font-weight:700;width:100%;margin-bottom:10px;"
              )
            ),
            shiny::actionButton(
              "fly_uk", "Met Office Stations",
              icon  = shiny::icon("map"),
              class = "nhm-btn",
              style = paste0(
                "background:", cols$cyan, ";color:", cols$deep, ";",
                "border:none;border-radius:4px;padding:10px 20px;",
                "font-weight:700;width:100%;margin-bottom:10px;"
              )
            ),
            shiny::actionButton(
              "fly_nhm", "Urban Research Station",
              icon  = shiny::icon("building-columns"),
              class = "nhm-btn",
              style = paste0(
                "background:", cols$cyan, ";color:", cols$deep, ";",
                "border:none;border-radius:4px;padding:10px 20px;",
                "font-weight:700;width:100%;margin-bottom:10px;"
              )
            ),
            shiny::hr(),
            shiny::tags$p(class = "nhm-value-label", "CURRENT VIEW"),
            shiny::tags$p(
              style = paste0(
                "font-size:1.3rem;font-weight:700;color:",
                cols$cyan, ";margin:4px 0;"
              ),
              shiny::textOutput("fly_status", inline = TRUE)
            )
          ),
          nhm_panel(
            title = "Mean Distance Between Devices",
            shiny::tags$div(
              style = "margin-bottom:10px;",
              shiny::tags$p(class = "nhm-value-label", "WORLD"),
              shiny::tags$p(
                style = paste0(
                  "font-size:1.5rem;font-weight:700;color:",
                  cols$pink, ";margin:2px 0;"
                ),
                paste0(dist_world, " km")
              )
            ),
            shiny::tags$div(
              style = "margin-bottom:10px;",
              shiny::tags$p(class = "nhm-value-label", "MET OFFICE"),
              shiny::tags$p(
                style = paste0(
                  "font-size:1.5rem;font-weight:700;color:",
                  cols$cyan, ";margin:2px 0;"
                ),
                paste0(dist_uk, " km")
              )
            ),
            shiny::tags$div(
              shiny::tags$p(class = "nhm-value-label", "NHM SENSORS"),
              shiny::tags$p(
                style = paste0(
                  "font-size:1.5rem;font-weight:700;color:",
                  cols$lime, ";margin:2px 0;"
                ),
                paste0(dist_nhm, " m")
              )
            )
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Map",
            plotly::plotlyOutput("fly_map", height = "650px")
          )
        )
      )
    )
  )
)

# ── Server ──────────────────────────────────────────────────────
server <- function(input, output, session) {

  year_data <- reactive({
    heat[heat$year == input$year, ]
  })

  output$year_display <- renderText({
    input$year
  })

  is_change <- reactive({ input$mode == "change" })

  output$heat_map <- plotly::renderPlotly({
    df <- year_data()

    if (is_change()) {
      nhm_world_map(
        palette       = palette,
        data          = df,
        lat           = ~lat,
        lon           = ~lon,
        marker_values = ~temp_change,
        colour_limits = range(heat$temp_change, na.rm = TRUE),
        ramp_colours  = c("#2166AC", "#67A9CF", "#F7F7F7",
                           "#EF8A62", "#B2182B", "#67001F"),
        show_colorbar  = TRUE,
        colorbar_title = "Change (\u00b0C)",
        marker_size   = 6,
        hover_size    = 14,
        label         = ~paste0(city_name, ": ",
                                ifelse(temp_change >= 0, "+", ""),
                                round(temp_change, 1),
                                "\u00b0C"),
        customdata    = ~paste0(
          "<b>", city_name, "</b>",
          "<br>Country: ", country,
          "<br>Change: ", ifelse(temp_change >= 0, "+", ""),
          round(temp_change, 1), "\u00b0C",
          "<br>Year: ", year
        )
      )
    } else {
      nhm_world_map(
        palette       = palette,
        data          = df,
        lat           = ~lat,
        lon           = ~lon,
        marker_values = ~hottest_3mo_tasmax_c,
        colour_limits = range(heat$hottest_3mo_tasmax_c, na.rm = TRUE),
        ramp_colours  = c("#2166AC", "#67A9CF", "#FDDBC7",
                           "#EF8A62", "#B2182B", "#67001F"),
        show_colorbar  = TRUE,
        colorbar_title = "\u00b0C",
        marker_size   = 6,
        hover_size    = 14,
        label         = ~paste0(city_name, ": ",
                                round(hottest_3mo_tasmax_c, 1),
                                "\u00b0C"),
        customdata    = ~paste0(
          "<b>", city_name, "</b>",
          "<br>Country: ", country,
          "<br>Peak temp: ", round(hottest_3mo_tasmax_c, 1), "\u00b0C",
          "<br>Year: ", year
        )
      )
    }
  })

  selected_city <- shiny::reactiveVal(NULL)

  shiny::observeEvent(plotly::event_data("plotly_click", source = "A"), {
    click <- plotly::event_data("plotly_click", source = "A")
    if (!is.null(click)) {
      # Find the city name from the current year's data
      df <- year_data()
      idx <- click$pointNumber + 1L
      if (idx >= 1 && idx <= nrow(df)) {
        selected_city(df$city_name[idx])
      }
    }
  })

  output$city_detail <- shiny::renderUI({
    click <- plotly::event_data("plotly_click", source = "A")
    if (is.null(click)) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "Click a city on the map."
      ))
    }
    shiny::HTML(click$customdata)
  })

  output$city_timeseries <- plotly::renderPlotly({
    city <- selected_city()
    if (is.null(city)) return(plotly::plotly_empty())

    city_df <- heat[heat$city_name == city, ]
    city_df <- city_df[order(city_df$year), ]

    y_col <- if (is_change()) "temp_change" else "hottest_3mo_tasmax_c"
    y_lab <- if (is_change()) "Change (\u00b0C)" else "\u00b0C"

    plotly::plot_ly(
      data   = city_df,
      x      = ~year,
      y      = as.formula(paste0("~", y_col)),
      type   = "scatter",
      mode   = "lines+markers",
      line   = list(color = cols$cyan, width = 2),
      marker = list(color = cols$cyan, size = 4),
      hoverinfo = "text",
      text   = ~paste0(year, ": ",
                       if (is_change()) paste0(ifelse(temp_change >= 0, "+", ""),
                                               round(temp_change, 1))
                       else round(hottest_3mo_tasmax_c, 1),
                       "\u00b0C")
    ) |>
      plotly::layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        xaxis = list(
          title = "",
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        ),
        yaxis = list(
          title = y_lab,
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        ),
        margin = list(l = 40, r = 10, t = 10, b = 30),
        showlegend = FALSE
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$year_stats <- shiny::renderUI({
    df <- year_data()

    if (is_change()) {
      avg  <- round(mean(df$temp_change, na.rm = TRUE), 1)
      top  <- round(max(df$temp_change, na.rm = TRUE), 1)
      sign_avg <- ifelse(avg >= 0, "+", "")
      sign_top <- ifelse(top >= 0, "+", "")

      return(shiny::tagList(
        shiny::tags$div(
          style = "margin-bottom:10px;",
          shiny::tags$p(class = "nhm-value-label", "MEAN CHANGE"),
          shiny::tags$p(
            style = paste0("font-size:1.5rem;font-weight:700;color:",
                           cols$cyan, ";margin:2px 0;"),
            paste0(sign_avg, avg, "\u00b0C")
          )
        ),
        shiny::tags$div(
          style = "margin-bottom:10px;",
          shiny::tags$p(class = "nhm-value-label", "MAX CHANGE"),
          shiny::tags$p(
            style = paste0("font-size:1.5rem;font-weight:700;color:",
                           cols$pink, ";margin:2px 0;"),
            paste0(sign_top, top, "\u00b0C")
          )
        )
      ))
    }

    avg  <- round(mean(df$hottest_3mo_tasmax_c, na.rm = TRUE), 1)
    top  <- round(max(df$hottest_3mo_tasmax_c, na.rm = TRUE), 1)
    hot  <- sum(df$hottest_3mo_tasmax_c >= 40, na.rm = TRUE)

    shiny::tagList(
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "GLOBAL MEAN"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:",
                         cols$cyan, ";margin:2px 0;"),
          paste0(avg, "\u00b0C")
        )
      ),
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "HOTTEST CITY"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:",
                         cols$pink, ";margin:2px 0;"),
          paste0(top, "\u00b0C")
        )
      ),
      shiny::tags$div(
        shiny::tags$p(class = "nhm-value-label", "CITIES \u2265 40\u00b0C"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:",
                         cols$lime, ";margin:2px 0;"),
          hot
        )
      )
    )
  })

  # ── Page 2: Fly-to NHM ──────────────────────────────────────

  fly_view <- shiny::reactiveVal("Globe")

  output$fly_status <- shiny::renderText({ fly_view() })

  # Custom dark mapbox style — boundaries loaded by URL (async), not inline
  dark_style <- list(
    version = 8L,
    sources = list(
      `carto-dark` = list(
        type     = "raster",
        tiles    = list(
          "https://basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"
        ),
        tileSize = 256L
      ),
      `world-110m` = list(
        type = "geojson",
        data = "geodata/world_110m.geojson"
      ),
      `world-50m` = list(
        type = "geojson",
        data = "geodata/world_50m.geojson"
      ),
      `uk-states` = list(
        type = "geojson",
        data = "geodata/united_kingdom_states.geojson"
      )
    ),
    layers  = list(
      list(id = "background", type = "background",
           paint = list(`background-color` = cols$card)),
      list(id = "world-110m-lines", type = "line",
           source = "world-110m",
           maxzoom = 4L,
           paint = list(`line-color` = cols$cyan,
                        `line-width` = 0.8,
                        `line-opacity` = 0.7)),
      list(id = "world-50m-lines", type = "line",
           source = "world-50m",
           minzoom = 4L, maxzoom = 7L,
           paint = list(`line-color` = cols$cyan,
                        `line-width` = 0.8,
                        `line-opacity` = 0.7)),
      list(id = "uk-states-lines", type = "line",
           source = "uk-states",
           minzoom = 7L,
           paint = list(`line-color` = cols$blue,
                        `line-width` = 1,
                        `line-opacity` = 0.7)),
      list(id = "carto-tiles", type = "raster",
           source = "carto-dark",
           minzoom = 11L,
           paint = list(`raster-opacity` = 1))
    )
  )

  # Mapbox outline map with NHM-themed GeoJSON boundaries

  output$fly_map <- plotly::renderPlotly({
    p <- plotly::plot_ly() |>
      # Trace 0: Met Office stations (hidden initially)
      plotly::add_trace(
        type = "scattermapbox",
        mode = "markers",
        data = stations,
        lat  = ~lat,
        lon  = ~lon,
        marker = list(
          size    = 6,
          color   = cols$cyan,
          opacity = 0.8
        ),
        text = ~paste0(station_name, " (", type, ")"),
        hoverinfo = "text",
        visible = FALSE,
        showlegend = FALSE
      ) |>
      # Trace 1: NHM sensors (hidden initially)
      plotly::add_trace(
        type = "scattermapbox",
        mode = "markers",
        data = sensors,
        lat  = ~lat,
        lon  = ~lon,
        marker = list(
          size    = 8,
          color   = cols$lime,
          opacity = 0.9
        ),
        text = ~sensor_name,
        hoverinfo = "text",
        visible = FALSE,
        showlegend = FALSE
      ) |>
      # Trace 2: WMO stations (visible initially — world view)
      plotly::add_trace(
        type = "scattermapbox",
        mode = "markers",
        data = wmo_stations,
        lat  = ~lat,
        lon  = ~lon,
        marker = list(
          size    = 3,
          color   = cols$pink,
          opacity = 0.6
        ),
        text = ~station_name,
        hoverinfo = "text",
        visible = TRUE,
        showlegend = FALSE
      )

    nhm_plotly_layout(p,
      palette = palette,
      mapbox = list(
        style  = dark_style,
        zoom   = 1,
        center = list(lat = 20, lon = 0)
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
  })

  # Helper: toggle station visibility via plotly proxy
  fly_proxy <- NULL
  shiny::observe({
    fly_proxy <<- plotly::plotlyProxy("fly_map", session)
  })

  show_stations <- function(visible) {
    plotly::plotlyProxyInvoke(
      fly_proxy, "restyle",
      list(visible = visible), list(0L)
    )
  }

  show_sensors <- function(visible) {
    plotly::plotlyProxyInvoke(
      fly_proxy, "restyle",
      list(visible = visible), list(1L)
    )
  }

  show_wmo <- function(visible) {
    plotly::plotlyProxyInvoke(
      fly_proxy, "restyle",
      list(visible = visible), list(2L)
    )
  }

  # World view
  shiny::observeEvent(input$fly_world, {
    fly_view("World")
    show_wmo(TRUE)
    nhm_map_flyto(session, "fly_map",
                  lat = 20, lon = 0, zoom = 1,
                  duration = 3500)
    later::later(function() {
      show_stations(FALSE)
      show_sensors(FALSE)
    }, delay = 3.5)
  })

  # UK view
  shiny::observeEvent(input$fly_uk, {
    fly_view("Met Office Stations")
    show_stations(TRUE)
    nhm_map_flyto(session, "fly_map",
                  lat = 54.5, lon = -3, zoom = 4.8,
                  duration = 3500)
    later::later(function() {
      show_sensors(FALSE)
      show_wmo(FALSE)
    }, delay = 3.5)
  })

  # NHM view
  shiny::observeEvent(input$fly_nhm, {
    fly_view("Urban Research Station")
    show_sensors(TRUE)
    nhm_map_flyto(session, "fly_map",
                  lat = 51.4965, lon = -0.1764, zoom = 17,
                  duration = 3500)
    later::later(function() {
      show_stations(FALSE)
      show_wmo(FALSE)
    }, delay = 3.5)
  })

  # London data for info panel
  output$london_info <- shiny::renderUI({
    london <- heat[heat$city_name == "London", ]
    if (nrow(london) == 0) {
      return(shiny::tags$p("London not found in dataset."))
    }
    now  <- london[london$year == min(years), ]
    last <- london[london$year == max(years), ]
    shiny::tagList(
      shiny::tags$p(paste0(
        "Peak temp ", min(years), ": ",
        round(now$hottest_3mo_tasmax_c, 1), "\u00b0C"
      )),
      shiny::tags$p(paste0(
        "Peak temp ", max(years), ": ",
        round(last$hottest_3mo_tasmax_c, 1), "\u00b0C"
      )),
      shiny::tags$p(
        style = paste0("color:", cols$pink, ";"),
        paste0("Change: +",
               round(last$hottest_3mo_tasmax_c - now$hottest_3mo_tasmax_c, 1),
               "\u00b0C")
      )
    )
  })
}

shinyApp(ui, server)
