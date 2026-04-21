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

# ── Log-scale positions for distance visualisation ───────────
log_x_min <- 15; log_x_max <- 205
log_min <- 1; log_max <- 5.5  # log10(10m) to log10(~316km)
log_ppd <- (log_x_max - log_x_min) / (log_max - log_min)
log_pos <- function(m) log_x_min + (log10(m) - log_min) * log_ppd
# Tick positions
tk_10m   <- round(log_pos(10), 1)
tk_100m  <- round(log_pos(100), 1)
tk_1km   <- round(log_pos(1000), 1)
tk_10km  <- round(log_pos(10000), 1)
tk_100km <- round(log_pos(100000), 1)
# Data positions
xp_nhm   <- round(log_pos(dist_nhm), 1)
xp_uk    <- round(log_pos(dist_uk * 1000), 1)
xp_world <- round(log_pos(dist_world * 1000), 1)

# ── Linear-scale positions for page 3 ───────────────────────────
lin_x_min <- 30; lin_x_max <- 1170
lin_max_km <- dist_world
lin_ppk   <- (lin_x_max - lin_x_min) / lin_max_km  # px per km
lp_nhm    <- round(lin_x_min + (dist_nhm / 1000) * lin_ppk, 1)
lp_uk     <- round(lin_x_min + dist_uk * lin_ppk, 1)
lp_world  <- round(lin_x_min + dist_world * lin_ppk, 1)
# Tick positions (linear)
lt_50km   <- round(lin_x_min + 50 * lin_ppk, 1)
lt_100km  <- round(lin_x_min + 100 * lin_ppk, 1)
lt_200km  <- round(lin_x_min + 200 * lin_ppk, 1)
lp_end    <- round(lin_x_min + lin_max_km * lin_ppk, 1)

# ── UI helpers ──────────────────────────────────────────────────
# Wrap repeated scattermapbox trace construction
add_map_trace <- function(p, data, text, colour, size, opacity,
                          visible = TRUE) {
  plotly::add_trace(p,
    type       = "scattermapbox",
    mode       = "markers",
    data       = data,
    lat        = ~lat,
    lon        = ~lon,
    marker     = list(size = size, color = colour, opacity = opacity),
    text       = text,
    hoverinfo  = "text",
    visible    = visible,
    showlegend = FALSE
  )
}

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
      title = "Scales of measurement",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Measurement Locations",
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
            shiny::HTML(sprintf('
              <svg viewBox="0 0 220 115" width="100%%"
                   xmlns="http://www.w3.org/2000/svg"
                   style="display:block;margin:0 auto;">
                <!-- Scale bar line -->
                <line x1="%s" y1="55" x2="%s" y2="55"
                  stroke="white" stroke-width="1" opacity="0.2"/>
                <!-- Major ticks: 10m, 10km, 100km -->
                <line x1="%s" y1="49" x2="%s" y2="61"
                  stroke="white" stroke-width="1" opacity="0.35"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="8"
                  text-anchor="middle" opacity="0.5">10 m</text>
                <line x1="%s" y1="49" x2="%s" y2="61"
                  stroke="white" stroke-width="1" opacity="0.35"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="8"
                  text-anchor="middle" opacity="0.5">10 km</text>
                <line x1="%s" y1="49" x2="%s" y2="61"
                  stroke="white" stroke-width="1" opacity="0.35"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="8"
                  text-anchor="middle" opacity="0.5">100 km</text>
                <!-- Minor ticks: 100m, 1km -->
                <line x1="%s" y1="51" x2="%s" y2="59"
                  stroke="white" stroke-width="0.7" opacity="0.2"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="7"
                  text-anchor="middle" opacity="0.35">100 m</text>
                <line x1="%s" y1="51" x2="%s" y2="59"
                  stroke="white" stroke-width="0.7" opacity="0.2"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="7"
                  text-anchor="middle" opacity="0.35">1 km</text>
                <!-- NHM marker -->
                <line x1="%s" y1="34" x2="%s" y2="50"
                  stroke="%s" stroke-width="1.5" opacity="0.5"/>
                <circle cx="%s" cy="55" r="5"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="1.5"/>
                <text x="%s" y="26" fill="%s"
                  font-family="sans-serif" font-size="10"
                  font-weight="700" text-anchor="middle">NHM</text>
                <text x="%s" y="16" fill="%s"
                  font-family="sans-serif" font-size="9"
                  font-weight="600" text-anchor="middle">%s m</text>
                <!-- Met Office marker (label below) -->
                <line x1="%s" y1="60" x2="%s" y2="76"
                  stroke="%s" stroke-width="1.5" opacity="0.5"/>
                <circle cx="%s" cy="55" r="5"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="1.5"/>
                <text x="%s" y="88" fill="%s"
                  font-family="sans-serif" font-size="10"
                  font-weight="700" text-anchor="middle">Met Office</text>
                <text x="%s" y="98" fill="%s"
                  font-family="sans-serif" font-size="9"
                  font-weight="600" text-anchor="middle">%s km</text>
                <!-- World marker -->
                <line x1="%s" y1="34" x2="%s" y2="50"
                  stroke="%s" stroke-width="1.5" opacity="0.5"/>
                <circle cx="%s" cy="55" r="5"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="1.5"/>
                <text x="%s" y="26" fill="%s"
                  font-family="sans-serif" font-size="10"
                  font-weight="700" text-anchor="end">World</text>
                <text x="%s" y="16" fill="%s"
                  font-family="sans-serif" font-size="9"
                  font-weight="600" text-anchor="end">%s km</text>
                <!-- Scale annotation -->
                <text x="110" y="112" fill="white"
                  font-family="sans-serif" font-size="7.5"
                  text-anchor="middle" opacity="0.35">logarithmic scale</text>
              </svg>',
              log_x_min, log_x_max,
              tk_10m, tk_10m, tk_10m,
              tk_10km, tk_10km, tk_10km,
              tk_100km, tk_100km, tk_100km,
              tk_100m, tk_100m, tk_100m,
              tk_1km, tk_1km, tk_1km,
              xp_nhm, xp_nhm, cols$lime,
              xp_nhm, cols$lime, cols$lime,
              xp_nhm, cols$lime,
              xp_nhm, cols$lime, dist_nhm,
              xp_uk, xp_uk, cols$cyan,
              xp_uk, cols$cyan, cols$cyan,
              xp_uk, cols$cyan,
              xp_uk, cols$cyan, dist_uk,
              xp_world, xp_world, cols$pink,
              xp_world, cols$pink, cols$pink,
              xp_world, cols$pink,
              xp_world, cols$pink, dist_world
            ))
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
    ),

    # ── Page 3: Linear distance scale ──────────────────────────
    nhm_flipbook_page(
      title = "Linear Scale",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            title = "Mean Distance Between Devices (linear scale)",
            shiny::HTML(sprintf('
              <svg viewBox="0 0 1200 160" width="100%%"
                   xmlns="http://www.w3.org/2000/svg"
                   style="display:block;margin:0 auto;">
                <!-- Scale bar line -->
                <line x1="%s" y1="75" x2="%s" y2="75"
                  stroke="white" stroke-width="1.5" opacity="0.2"/>
                <!-- 0 km tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="11"
                  text-anchor="middle" opacity="0.5">0</text>
                <!-- 50 km tick -->
                <line x1="%s" y1="69" x2="%s" y2="81"
                  stroke="white" stroke-width="0.8" opacity="0.25"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="10"
                  text-anchor="middle" opacity="0.35">50 km</text>
                <!-- 100 km tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="11"
                  text-anchor="middle" opacity="0.5">100 km</text>
                <!-- 200 km tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="11"
                  text-anchor="middle" opacity="0.5">200 km</text>
                <!-- End tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <!-- NHM marker (essentially at zero) -->
                <line x1="%s" y1="48" x2="%s" y2="70"
                  stroke="%s" stroke-width="2" opacity="0.6"/>
                <circle cx="%s" cy="75" r="7"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="2"/>
                <text x="%s" y="38" fill="%s"
                  font-family="sans-serif" font-size="14"
                  font-weight="700" text-anchor="start">NHM</text>
                <text x="%s" y="24" fill="%s"
                  font-family="sans-serif" font-size="12"
                  font-weight="600" text-anchor="start">%s m</text>
                <!-- Met Office marker (below line) -->
                <line x1="%s" y1="80" x2="%s" y2="106"
                  stroke="%s" stroke-width="2" opacity="0.6"/>
                <circle cx="%s" cy="75" r="7"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="2"/>
                <text x="%s" y="120" fill="%s"
                  font-family="sans-serif" font-size="14"
                  font-weight="700" text-anchor="middle">Met Office</text>
                <text x="%s" y="134" fill="%s"
                  font-family="sans-serif" font-size="12"
                  font-weight="600" text-anchor="middle">%s km</text>
                <!-- World marker -->
                <line x1="%s" y1="48" x2="%s" y2="70"
                  stroke="%s" stroke-width="2" opacity="0.6"/>
                <circle cx="%s" cy="75" r="7"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="2"/>
                <text x="%s" y="38" fill="%s"
                  font-family="sans-serif" font-size="14"
                  font-weight="700" text-anchor="end">World</text>
                <text x="%s" y="24" fill="%s"
                  font-family="sans-serif" font-size="12"
                  font-weight="600" text-anchor="end">%s km</text>
                <!-- Scale annotation -->
                <text x="600" y="155" fill="white"
                  font-family="sans-serif" font-size="10"
                  text-anchor="middle" opacity="0.35">linear scale</text>
              </svg>',
              lin_x_min, lin_x_max,
              lin_x_min, lin_x_min, lin_x_min,
              lt_50km, lt_50km, lt_50km,
              lt_100km, lt_100km, lt_100km,
              lt_200km, lt_200km, lt_200km,
              lp_end, lp_end,
              lp_nhm, lp_nhm, cols$lime,
              lp_nhm, cols$lime, cols$lime,
              lp_nhm, cols$lime,
              lp_nhm, cols$lime, dist_nhm,
              lp_uk, lp_uk, cols$cyan,
              lp_uk, cols$cyan, cols$cyan,
              lp_uk, cols$cyan,
              lp_uk, cols$cyan, dist_uk,
              lp_world, lp_world, cols$pink,
              lp_world, cols$pink, cols$pink,
              lp_world, cols$pink,
              lp_world, cols$pink, dist_world
            ))
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

    common <- list(
      palette       = palette,
      data          = df,
      lat           = ~lat,
      lon           = ~lon,
      show_colorbar = TRUE,
      marker_size   = 6,
      hover_size    = 14
    )

    if (is_change()) {
      do.call(nhm_world_map, c(common, list(
        marker_values  = ~temp_change,
        colour_limits  = range(heat$temp_change, na.rm = TRUE),
        ramp_colours   = c("#2166AC", "#67A9CF", "#F7F7F7",
                           "#EF8A62", "#B2182B", "#67001F"),
        colorbar_title = "Change (\u00b0C)",
        label          = ~paste0(city_name, ": ",
                                 ifelse(temp_change >= 0, "+", ""),
                                 round(temp_change, 1), "\u00b0C"),
        customdata     = ~paste0(
          "<b>", city_name, "</b>",
          "<br>Country: ", country,
          "<br>Change: ", ifelse(temp_change >= 0, "+", ""),
          round(temp_change, 1), "\u00b0C",
          "<br>Year: ", year
        )
      )))
    } else {
      do.call(nhm_world_map, c(common, list(
        marker_values  = ~hottest_3mo_tasmax_c,
        colour_limits  = range(heat$hottest_3mo_tasmax_c, na.rm = TRUE),
        ramp_colours   = c("#2166AC", "#67A9CF", "#FDDBC7",
                           "#EF8A62", "#B2182B", "#67001F"),
        colorbar_title = "\u00b0C",
        label          = ~paste0(city_name, ": ",
                                 round(hottest_3mo_tasmax_c, 1), "\u00b0C"),
        customdata     = ~paste0(
          "<b>", city_name, "</b>",
          "<br>Country: ", country,
          "<br>Peak temp: ", round(hottest_3mo_tasmax_c, 1), "\u00b0C",
          "<br>Year: ", year
        )
      )))
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
      add_map_trace(stations,
        text    = ~paste0(station_name, " (", type, ")"),
        colour  = cols$cyan, size = 6, opacity = 0.8,
        visible = FALSE
      ) |>
      # Trace 1: NHM sensors (hidden initially)
      add_map_trace(sensors,
        text    = ~sensor_name,
        colour  = cols$lime, size = 8, opacity = 0.9,
        visible = FALSE
      ) |>
      # Trace 2: WMO stations (visible initially — world view)
      add_map_trace(wmo_stations,
        text    = ~station_name,
        colour  = cols$pink, size = 3, opacity = 0.6,
        visible = TRUE
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

  # Helper: toggle trace visibility via plotly proxy
  fly_proxy <- NULL
  shiny::observe({
    fly_proxy <<- plotly::plotlyProxy("fly_map", session)
  })

  make_show_fn <- function(idx) {
    function(visible) {
      plotly::plotlyProxyInvoke(fly_proxy, "restyle",
        list(visible = visible), list(idx))
    }
  }
  show_stations <- make_show_fn(0L)
  show_sensors  <- make_show_fn(1L)
  show_wmo      <- make_show_fn(2L)

  # Generic fly-to handler
  do_fly_to <- function(view_name, lat, lon, zoom, show_fn, hide_fns) {
    fly_view(view_name)
    show_fn(TRUE)
    nhm_map_flyto(session, "fly_map", lat = lat, lon = lon, zoom = zoom,
                  duration = 3500)
    later::later(function() {
      for (f in hide_fns) f(FALSE)
    }, delay = 3.5)
  }

  # World view
  shiny::observeEvent(input$fly_world, {
    do_fly_to("World", lat = 20, lon = 0, zoom = 1,
              show_fn   = show_wmo,
              hide_fns  = list(show_stations, show_sensors))
  })

  # UK view
  shiny::observeEvent(input$fly_uk, {
    do_fly_to("Met Office Stations", lat = 54.5, lon = -3, zoom = 4.8,
              show_fn   = show_stations,
              hide_fns  = list(show_sensors, show_wmo))
  })

  # NHM view
  shiny::observeEvent(input$fly_nhm, {
    do_fly_to("Urban Research Station", lat = 51.4965, lon = -0.1764, zoom = 17,
              show_fn   = show_sensors,
              hide_fns  = list(show_stations, show_wmo))
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
