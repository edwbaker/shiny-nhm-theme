# Demo app for the shinynhm theme — flipbook layout
# Run with: shiny::runApp("inst/demo")

library(shiny)
library(shinynhm)

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

    # ── Page 4: Data Table ──────────────────────────────────────
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

    # ── Page 5: Summary Stats ───────────────────────────────────
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
