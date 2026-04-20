# Demo app for the shinynhm theme — flipbook layout
# Run with: shiny::runApp("inst/demo")

library(shiny)
library(shinynhm)
library(ggplot2)
library(plotly)

# Change palette here: "default" or "nhm-web"
palette <- "default"

cols <- nhm_colours(palette)

ui <- nhm_page(
  title       = "Understanding Urban Heat Islands",
  subbrand    = "NATURAL HISTORY MUSEUM X AWS",
  description = "Exploring temperature fluctuations across a day in the Natural History Museum Gardens",
  footer      = FALSE,
  palette     = palette,

  nhm_slide_panel(
    id = "uhi_info",
    tab_label = "What is a UHI?",
    top = "40%",
    shiny::tags$h2("What is an Urban Heat Island?"),
    shiny::tags$p(
      "An ", shiny::tags$strong("urban heat island (UHI)"),
      " is a metropolitan area that is significantly warmer than its",
      " surrounding rural areas due to human activities and the built",
      " environment."
    ),
    shiny::tags$p(
      "Surfaces like concrete, asphalt and buildings absorb and re-emit",
      " heat, while reduced vegetation limits natural cooling through",
      " evapotranspiration. The effect is most pronounced at night, when",
      " urban materials slowly release stored heat."
    ),
    shiny::tags$p(
      "UHIs can raise city temperatures by ",
      shiny::tags$strong("1\u20133\u00B0C on average"),
      " \u2014 and by as much as ",
      shiny::tags$strong("12\u00B0C"),
      " during calm, clear evenings."
    ),
    shiny::tags$p(
      "This dashboard explores microclimate variation across the Natural",
      " History Museum grounds, comparing sensors in different settings",
      " \u2014 from shaded olive groves to exposed hillsides \u2014 to",
      " understand how local land cover and vegetation influence",
      " temperature at a hyperlocal scale."
    )
  ),

  nhm_slide_panel(
    id = "about_sensors",
    tab_label = "About the Sensors",
    top = "60%",
    shiny::tags$h2("Sensor Network"),
    shiny::tags$p(
      "The NHM gardens are equipped with ",
      shiny::tags$strong("24 soil temperature sensors"),
      " recording data every 30 seconds at 15 cm depth."
    ),
    shiny::tags$p(
      "Sensors are placed across contrasting micro-habitats to capture",
      " how land cover, shade, and proximity to hard surfaces influence",
      " soil temperature."
    ),
    shiny::tags$p(shiny::tags$strong("Key locations:")),
    shiny::tags$ul(
      shiny::tags$li("Courtyard olive grove"),
      shiny::tags$li("Wood pile hill (exposed)"),
      shiny::tags$li("10 cm, 15 cm, and 20 cm from concrete paths"),
      shiny::tags$li("Bare soil vs ivy-covered ground")
    ),
    shiny::tags$p(
      "All data is ingested and stored using ",
      shiny::tags$strong("AWS cloud infrastructure"),
      ", enabling scalable analysis of millions of readings."
    )
  ),

  nhm_flipbook(
    id = "main",

    # ── Page 1: The Initiative ──────────────────────────────────
    nhm_flipbook_page(
      title = "The Initiative",
      nhm_panel(
        title = "The Initiative",
        shiny::fluidRow(
          shiny::column(
            4,
            nhm_flip_card(
              front_title = "The Problem",
              tag = "URBAN HEAT",
              bg_image = "https://images.unsplash.com/photo-1513635269975-59663e0ac1ad?w=800&q=80",
              front = shiny::tagList(
              shiny::tags$p(class = "stat-highlight", "970 cities"),
              shiny::tags$p(class = "stat-caption",
                "projected to hit 35\u00B0C average summer highs by 2050"
              )
            ),
            back = shiny::tagList(
              shiny::tags$p(
                "As cities expand, ",
                shiny::tags$strong("urban heat islands"),
                " intensify\u2014concrete and asphalt absorb solar energy,",
                " raising temperatures by up to 10\u00B0C compared to",
                " surrounding green areas."
              ),
              shiny::tags$p(
                "This creates cascading effects on ",
                shiny::tags$strong("biodiversity"),
                ", public health, and energy consumption."
              )
            )
          )
        ),
        shiny::column(
          4,
          nhm_flip_card(
            front_title = "What We're Doing",
            tag = "RESEARCH",
            bg_image = "https://d2mzzbg81ytaaw.cloudfront.net/images/Figure%204%20Pond%20and%20sunken%20pathway%20%C2%A9%20The%20Trustees%20of%20the%20Natural%20History%20Museum,%20London.jpg",
            front = shiny::tagList(
              shiny::tags$p(class = "stat-highlight", "8 million+"),
              shiny::tags$p(class = "stat-caption",
                "temperature recordings across the NHM gardens"
              )
            ),
            back = shiny::tagList(
              shiny::tags$p(
                "A network of ",
                shiny::tags$strong("24 sensors"),
                " placed across the Museum\u2019s gardens records",
                " soil and surface temperatures every 30 seconds."
              ),
              shiny::tags$ul(
                shiny::tags$li("Soil at 15 cm depth"),
                shiny::tags$li("Surface temperature"),
                shiny::tags$li("Shaded vs exposed readings")
              )
            )
          )
        ),
        shiny::column(
          4,
          nhm_flip_card(
            front_title = "From Insight to Action",
            tag = "SOLUTIONS",
            bg_image = "https://d2mzzbg81ytaaw.cloudfront.net/images/An%20ecologist%20at%20work%204%20%C2%A9%20The%20Trustees%20of%20the%20Natural%20History%20Museum,%20London.jpg",
            front = shiny::tagList(
              shiny::tags$p(class = "stat-highlight", "Species level"),
              shiny::tags$p(class = "stat-caption",
                "fine-tuning strategies from plant selection to soil cover"
              )
            ),
            back = shiny::tagList(
              shiny::tags$p(
                "Data-driven decisions help identify which ",
                shiny::tags$strong("plant species"),
                " and ground covers are most effective at reducing",
                " local temperatures."
              ),
              shiny::tags$p(
                "Results inform urban greening strategies for ",
                shiny::tags$strong("cities worldwide"), "."
              )
            )
          )
        )
      )
      )
    ),

    # ── Page 2: Sensor Data ─────────────────────────────────────
    nhm_flipbook_page(
      title = "Sensor Data",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            selectInput("temp_sensor", "Sensor:",
                        choices = c("Courtyard Olive Grove",
                                    "Wood Pile Hill",
                                    "Path Proximity",
                                    "Ground Cover")),
            sliderInput("temp_hours", "Hour range:",
                        min = 0, max = 23, value = c(6, 18)),
            actionButton("temp_update", "Update", class = "btn-primary")
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Temperature Over Time",
            plotOutput("temp_plot", height = "400px")
          )
        )
      )
    ),

    # ── Page 3: Comparison ──────────────────────────────────────
    nhm_flipbook_page(
      title = "Comparison",
      shiny::fluidRow(
        shiny::column(
          6,
          nhm_panel(
            title = "Olive Grove",
            plotOutput("compare_plot1", height = "350px")
          )
        ),
        shiny::column(
          6,
          nhm_panel(
            title = "Exposed Hillside",
            plotOutput("compare_plot2", height = "350px")
          )
        )
      )
    ),

    # ── Page 4: ggplot2 Theme ────────────────────────────────────
    nhm_flipbook_page(
      title = "ggplot2 Theme",
      shiny::fluidRow(
        shiny::column(
          6,
          nhm_panel(
            title = "Line Chart",
            plotOutput("gg_line", height = "350px")
          )
        ),
        shiny::column(
          6,
          nhm_panel(
            title = "Bar Chart",
            plotOutput("gg_bar", height = "350px")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          nhm_panel(
            title = "Scatter Plot with Facets",
            plotOutput("gg_scatter", height = "350px")
          )
        ),
        shiny::column(
          6,
          nhm_panel(
            title = "Density Plot",
            plotOutput("gg_density", height = "350px")
          )
        )
      )
    ),

    # ── Page 5: Sensor Map ────────────────────────────────────
    nhm_flipbook_page(
      title = "Sensor Map",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            selectInput("map_hour", "Hour of day:",
                        choices = setNames(0:23,
                          paste0(sprintf("%02d", 0:23), ":00")),
                        selected = 14),
            shiny::tags$p(
              style = paste0("color:", cols$muted, "; font-size:0.85rem;"),
              "Marker size and colour reflect simulated soil temperature ",
              "at each sensor location."
            )
          ),
          nhm_panel(
            title = "Legend",
            shiny::tags$div(
              style = "display:flex; align-items:center; gap:8px; margin-bottom:6px;",
              shiny::tags$span(
                style = paste0("width:12px;height:12px;border-radius:50%;background:",
                               cols$cyan, ";display:inline-block;")),
              shiny::tags$span(style = paste0("color:", cols$text), "Cooler")
            ),
            shiny::tags$div(
              style = "display:flex; align-items:center; gap:8px; margin-bottom:6px;",
              shiny::tags$span(
                style = paste0("width:12px;height:12px;border-radius:50%;background:",
                               cols$lime, ";display:inline-block;")),
              shiny::tags$span(style = paste0("color:", cols$text), "Moderate")
            ),
            shiny::tags$div(
              style = "display:flex; align-items:center; gap:8px;",
              shiny::tags$span(
                style = paste0("width:12px;height:12px;border-radius:50%;background:",
                               cols$pink, ";display:inline-block;")),
              shiny::tags$span(style = paste0("color:", cols$text), "Warmer")
            )
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "NHM Gardens — Sensor Locations",
            plotly::plotlyOutput("sensor_map", height = "550px")
          )
        )
      )
    ),

    # ── Page 6: World Map ─────────────────────────────────────
    nhm_flipbook_page(
      title = "World Map",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "About",
            shiny::tags$p(
              "An interactive world map styled with NHM colours, ",
              "built using ", shiny::tags$code("nhm_world_map()"), "."
            ),
            shiny::tags$p(
              "Click a marker to see site details."
            ),
            selectInput("world_projection", "Projection:",
                        choices = c("natural earth", "equirectangular",
                                    "orthographic", "robinson",
                                    "mollweide"),
                        selected = "natural earth")
          ),
          nhm_panel(
            title = "Site Details",
            shiny::uiOutput("world_map_detail")
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Global Sensor Network",
            plotly::plotlyOutput("world_map", height = "550px")
          )
        )
      )
    ),

    # ── Page 7: Data Table ──────────────────────────────────────
    nhm_flipbook_page(
      title = "Data Table",
      shiny::fluidRow(
        shiny::column(
          4,
          nhm_panel(
            title = "Filter",
            selectInput("table_dataset", "Dataset:",
                        choices = c("mtcars", "iris", "pressure")),
            sliderInput("table_rows", "Rows:",
                        min = 5, max = 50, value = 15)
          )
        ),
        shiny::column(
          8,
          nhm_panel(
            title = "Preview",
            tableOutput("data_table")
          )
        )
      )
    ),

    # ── Page 8: Summary Stats ───────────────────────────────────
    nhm_flipbook_page(
      title = "Summary",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            shiny::tags$p(class = "nhm-value-label", "AVERAGE TEMP"),
            shiny::tags$p(
              style = paste0("font-size:2rem; font-weight:700; color:",
                             cols$cyan, "; margin:4px 0;"),
              "18.3\u00B0C"
            )
          )
        ),
        shiny::column(
          3,
          nhm_panel(
            shiny::tags$p(class = "nhm-value-label", "MAX RECORDED"),
            shiny::tags$p(
              style = paste0("font-size:2rem; font-weight:700; color:",
                             cols$lime, "; margin:4px 0;"),
              "34.1\u00B0C"
            )
          )
        ),
        shiny::column(
          3,
          nhm_panel(
            shiny::tags$p(class = "nhm-value-label", "SENSORS ACTIVE"),
            shiny::tags$p(
              style = paste0("font-size:2rem; font-weight:700; color:",
                             cols$pink, "; margin:4px 0;"),
              "24"
            )
          )
        ),
        shiny::column(
          3,
          nhm_panel(
            shiny::tags$p(class = "nhm-value-label", "UHI EFFECT"),
            shiny::tags$p(
              style = paste0("font-size:2rem; font-weight:700; color:",
                             cols$cyan, "; margin:4px 0;"),
              "+2.7\u00B0C"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ── World map (plotly) ──────────────────────────────────────
  output$world_map <- plotly::renderPlotly({
    sites <- data.frame(
      city = c("London", "Nairobi", "Sydney", "S\u00e3o Paulo",
               "New York", "Tokyo", "Cape Town", "Mumbai",
               "Berlin", "Mexico City", "Singapore", "Lima"),
      lat  = c(51.50, -1.29, -33.87, -23.55,
               40.71, 35.68, -33.92, 19.08,
               52.52, 19.43, 1.35, -12.05),
      lon  = c(-0.13, 36.82, 151.21, -46.63,
               -74.01, 139.69, 18.42, 72.88,
               13.41, -99.13, 103.82, -77.04),
      sensors = c(24, 18, 12, 15, 20, 22, 10, 16, 14, 11, 8, 9),
      status  = c("Active", "Active", "Maintenance", "Active",
                  "Active", "Active", "Planned", "Active",
                  "Active", "Planned", "Active", "Maintenance"),
      since   = c(2019, 2020, 2021, 2020, 2019, 2021,
                  2023, 2022, 2021, 2023, 2022, 2022),
      stringsAsFactors = FALSE
    )

    cols <- nhm_colours(palette)

    p <- nhm_world_map(
      palette    = palette,
      projection = input$world_projection,
      data       = sites,
      lat        = ~lat,
      lon        = ~lon,
      label      = ~city,
      customdata = ~paste0(
        "<b>", city, "</b>",
        "<br>Sensors: ", sensors,
        "<br>Status: ", status,
        "<br>Since: ", since
      )
    )
    p
  })

  # Click handler for world map
  output$world_map_detail <- shiny::renderUI({
    click <- plotly::event_data("plotly_click", source = "A")
    if (is.null(click)) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "Click a marker on the map to see details."
      ))
    }
    shiny::HTML(click$customdata)
  })

  # ── Sensor map (plotly) ──────────────────────────────────────
  # Simulated sensor positions across the NHM Wildlife Garden
  sensor_data <- data.frame(
    id   = paste0("S", sprintf("%02d", 1:12)),
    name = c("Courtyard Olive N", "Courtyard Olive S",
             "Wood Pile Summit", "Wood Pile Base",
             "Path Edge 10cm", "Path Edge 20cm",
             "Bare Soil A", "Bare Soil B",
             "Ivy Cover East", "Ivy Cover West",
             "Pond Margin", "Meadow Centre"),
    lat  = c(51.49580, 51.49572, 51.49555, 51.49548,
             51.49565, 51.49560, 51.49545, 51.49540,
             51.49575, 51.49570, 51.49535, 51.49550),
    lon  = c(-0.17620, -0.17610, -0.17580, -0.17575,
             -0.17640, -0.17635, -0.17600, -0.17595,
             -0.17560, -0.17555, -0.17615, -0.17570),
    # base temperatures differ by microhabitat
    base = c(16, 16.5, 20, 18, 19, 18.5,
             17.5, 17, 15.5, 15, 14.5, 17),
    amp  = c(5, 5.5, 9, 7, 8, 7.5,
             6, 5.5, 4, 4.5, 3.5, 6.5),
    stringsAsFactors = FALSE
  )

  output$sensor_map <- plotly::renderPlotly({
    hr <- as.numeric(input$map_hour)
    df <- sensor_data
    df$temp <- round(df$base + df$amp * sin((hr - 6) * pi / 12), 1)

    pal  <- nhm_palette()
    cols <- nhm_colours(palette)

    # colour ramp: cyan -> lime -> pink
    ramp <- grDevices::colorRampPalette(c(cols$cyan, cols$lime, cols$pink))
    temp_range <- range(df$temp)
    temp_norm  <- (df$temp - temp_range[1]) /
                  max(temp_range[2] - temp_range[1], 0.1)
    n_cols <- 100
    ramp_cols <- ramp(n_cols)
    df$colour <- ramp_cols[pmax(1, ceiling(temp_norm * (n_cols - 1)) + 1)]

    # marker size scaled by temperature
    df$size <- 12 + 18 * temp_norm

    p <- plotly::plot_ly(
      df,
      type = "scattermapbox",
      lat  = ~lat,
      lon  = ~lon,
      mode = "markers",
      marker = list(
        size    = df$size,
        color   = df$colour,
        opacity = 0.9
      ),
      text = ~paste0(
        "<b>", name, "</b> (", id, ")",
        "<br>Temp: ", temp, "\u00b0C",
        "<br>Hour: ", sprintf("%02d:00", hr)
      ),
      hoverinfo = "text"
    )

    nhm_plotly_layout(p,
      palette = palette,
      mapbox = list(
        style  = "open-street-map",
        zoom   = 17,
        center = list(lat = 51.4956, lon = -0.1760)
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
  })

  # ── ggplot2 demo plots ──────────────────────────────────────
  output$gg_line <- renderPlot({
    hours <- seq(0, 23, length.out = 48)
    df <- data.frame(
      hour = rep(hours, 2),
      temp = c(14 + 6 * sin((hours - 6) * pi / 12) + rnorm(48, 0, 0.3),
               16 + 9 * sin((hours - 5) * pi / 12) + rnorm(48, 0, 0.3)),
      sensor = rep(c("Olive Grove", "Exposed Hill"), each = 48)
    )
    ggplot(df, aes(hour, temp, colour = sensor)) +
      geom_line(linewidth = 1) +
      scale_colour_nhm() +
      labs(title = "Temperature Over 24 Hours",
           x = "Hour of Day", y = "Temperature (\u00b0C)",
           colour = NULL) +
      theme_nhm(palette = palette)
  })

  output$gg_bar <- renderPlot({
    df <- data.frame(
      location = c("Olive Grove", "Wood Pile", "Path Edge",
                   "Bare Soil", "Ivy Cover"),
      mean_temp = c(17.2, 21.4, 19.8, 18.5, 16.1)
    )
    df$location <- factor(df$location, levels = df$location)
    ggplot(df, aes(location, mean_temp, fill = location)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      scale_fill_nhm() +
      labs(title = "Mean Daytime Temperature by Location",
           x = NULL, y = "Temperature (\u00b0C)") +
      theme_nhm(palette = palette)
  })

  output$gg_scatter <- renderPlot({
    df <- iris
    ggplot(df, aes(Sepal.Length, Sepal.Width, colour = Species)) +
      geom_point(size = 2, alpha = 0.8) +
      scale_colour_nhm() +
      facet_wrap(~Species) +
      labs(title = "Iris Measurements",
           x = "Sepal Length", y = "Sepal Width") +
      theme_nhm(palette = palette) +
      theme(legend.position = "none")
  })

  output$gg_density <- renderPlot({
    hours <- seq(0, 23, length.out = 200)
    df <- data.frame(
      temp = c(14 + 6 * sin((hours - 6) * pi / 12) + rnorm(200, 0, 1.5),
               16 + 9 * sin((hours - 5) * pi / 12) + rnorm(200, 0, 1.5)),
      sensor = rep(c("Shaded", "Exposed"), each = 200)
    )
    ggplot(df, aes(temp, fill = sensor)) +
      geom_density(alpha = 0.6, colour = NA) +
      scale_fill_nhm() +
      labs(title = "Temperature Distribution",
           x = "Temperature (\u00b0C)", y = "Density",
           fill = NULL) +
      theme_nhm(palette = palette)
  })

  # Temperature panel plot
  output$temp_plot <- renderPlot({
    hours <- seq(input$temp_hours[1], input$temp_hours[2], length.out = 50)
    temp  <- 15 + 8 * sin((hours - 6) * pi / 12) + rnorm(50, 0, 0.5)
    pal   <- nhm_palette()

    nhm_par()
    plot(hours, temp, type = "l", lwd = 2, col = pal[1],
         xlab = "Hour of Day", ylab = "Temperature (\u00B0C)",
         main = input$temp_sensor,
         panel.first = nhm_grid())
    points(hours, temp, pch = 19, cex = 0.6, col = pal[1])
  }) |> bindEvent(input$temp_sensor, input$temp_hours, input$temp_update)

  # Comparison plots
  output$compare_plot1 <- renderPlot({
    hours <- 0:23
    pal   <- nhm_palette()

    nhm_par()
    y1 <- 14 + 6 * sin((hours - 6) * pi / 12)
    y2 <- 15 + 8 * sin((hours - 5) * pi / 12)
    plot(hours, y1,
         type = "l", lwd = 2, col = pal[1],
         ylim = range(y1, y2),
         xlab = "Hour", ylab = "\u00B0C", main = "Soil 15cm vs Surface",
         panel.first = nhm_grid())
    lines(hours, y2, lwd = 2, col = pal[2])
    nhm_legend("topright", legend = c("Soil 15cm", "Surface"),
               col = pal[1:2], lwd = 2)
  })

  output$compare_plot2 <- renderPlot({
    hours <- 0:23
    pal   <- nhm_palette()

    nhm_par()
    plot(hours, 16 + 10 * sin((hours - 5) * pi / 12),
         type = "l", lwd = 2, col = pal[3],
         xlab = "Hour", ylab = "\u00B0C", main = "Surface vs Shaded",
         panel.first = nhm_grid())
    lines(hours, 14 + 5 * sin((hours - 7) * pi / 12),
          lwd = 2, col = pal[1])
    nhm_legend("topright", legend = c("Surface", "Shaded"),
               col = pal[c(3, 1)], lwd = 2)
  })

  # Data table
  output$data_table <- renderTable({
    d <- switch(input$table_dataset,
      "mtcars"   = mtcars,
      "iris"     = iris,
      "pressure" = pressure
    )
    head(d, input$table_rows)
  })
}

shinyApp(ui, server)
