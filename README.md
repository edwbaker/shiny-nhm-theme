# shinynhm

## Quick Start

```r
library(shiny)
library(shinynhm)

ui <- nhm_page(
  title    = "My Dashboard Title",
  subbrand = "NATURAL HISTORY MUSEUM",

  fluidRow(
    column(6, nhm_card(title = "Card 1", p("Content here"))),
    column(6, nhm_card(title = "Card 2", p("More content")))
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
```

## Functions

| Function | Description |
|---|---|
| `nhm_page()` | Full themed page with header, logo, and optional footer |
| `nhm_header()` | Standalone header bar with logo and configurable text |
| `nhm_theme()` | HTML dependency that loads the NHM CSS (for use in custom layouts) |
| `nhm_card()` | Styled card/panel container |
| `nhm_colors()` | Named list of theme hex colours for use in plots |

## Customising the Header

Both `nhm_page()` and `nhm_header()` accept configurable text:

```r
nhm_page(
  title        = "Understanding Urban Heat Islands",
  subbrand     = "NATURAL HISTORY MUSEUM X AWS",
  logo_height  = "48px",
  right_header = actionButton("info", "About", class = "btn-primary"),
  footer       = TRUE
)
```

- **`title`** — main heading displayed in the header bar
- **`subbrand`** — smaller uppercase text above the title (set to `NULL` to hide)
- **`right_header`** — any Shiny UI to place on the right side of the header

## Using with `navbarPage()`

If you prefer `navbarPage()`, attach the theme dependency directly:

```r
ui <- navbarPage(
  "My App",
  header = nhm_theme(),
  tabPanel("Tab 1", nhm_card(title = "Hello", p("World"))),
  tabPanel("Tab 2", p("More content"))
)
```

## Colour Palette

Access theme colours for plots and custom UI:

```r
cols <- nhm_colors()
plot(1:10, col = cols$accent, pch = 19)
```

## Demo App

Run the included demo:

```r
shiny::runApp(system.file("demo", package = "shinynhm"))
```

## License

See file LICENSE for details.
