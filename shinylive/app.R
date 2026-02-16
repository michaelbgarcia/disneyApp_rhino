library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

rides_raw <- read.csv(
  file = "data/rides.csv",
  stringsAsFactors = FALSE
) |>
  mutate(
    status = factor(status, levels = c("OPERATING", "DOWN")),
    wait_time = as.integer(wait_time),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

status_palette <- c(
  OPERATING = "#0063F7",
  DOWN = "#FF7A18"
)

simulate_snapshot <- function(df) {
  n <- nrow(df)
  df |>
    mutate(
      wait_time = pmax(5L, round(wait_time * runif(n, 0.7, 1.3) / 5) * 5),
      status = if_else(runif(n) <= 0.12, "DOWN", "OPERATING"),
      last_updated = format(Sys.time() - sample(0:1800, n, replace = TRUE), "%Y-%m-%d %H:%M:%S")
    )
}

initial_snapshot <- simulate_snapshot(rides_raw)
park_choices <- sort(unique(initial_snapshot$parks_name))
wait_limits <- range(initial_snapshot$wait_time, na.rm = TRUE)

theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  primary = "#0063F7",
  base_font = font_google("Atkinson Hyperlegible")
)

ui <- page_sidebar(
  title = "Disney Wait Snapshot (Shinylive)",
  theme = theme,
  head_content = tags$link(rel = "stylesheet", href = "styles.css"),
  sidebar = sidebar(
    selectInput(
      inputId = "park_select",
      label = "Park",
      choices = park_choices,
      selected = park_choices[1]
    ),
    sliderInput(
      inputId = "wait_filter",
      label = "Wait time (minutes)",
      min = wait_limits[1],
      max = wait_limits[2],
      value = wait_limits,
      step = 5
    ),
    actionButton(
      inputId = "refresh",
      label = "Refresh snapshot",
      icon = icon("arrows-rotate"),
      width = "100%"
    ),
    helpText("This Shinylive demo uses a static snapshot seeded from parks_data.rds and simulates live updates browser-side.")
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      card_header("Ride Detail"),
      tableOutput("ride_table")
    ),
    card(
      full_screen = TRUE,
      card_header("Ride Locations"),
      plotOutput("map_plot", height = "420px")
    )
  )
)

server <- function(input, output, session) {
  snapshot_state <- reactiveVal(initial_snapshot)

  observeEvent(input$refresh, {
    snapshot_state(simulate_snapshot(rides_raw))
  })

  filtered_data <- reactive({
    snapshot_state() |>
      filter(
        parks_name %in% input$park_select,
        wait_time >= input$wait_filter[1],
        wait_time <= input$wait_filter[2]
      ) |>
      arrange(desc(wait_time))
  })

  output$ride_table <- renderTable({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No rides match the current filters."))

    df |>
      select(
        Ride = name,
        Status = status,
        `Wait (min)` = wait_time,
        `Next LL` = ll_time,
        `Last Updated` = last_updated
      )
  },
  striped = TRUE,
  bordered = FALSE,
  spacing = "m",
  width = "100%"
  )

  output$map_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No rides to display."))

    ggplot(df, aes(x = longitude, y = latitude)) +
      geom_point(aes(color = status, size = wait_time), alpha = 0.85, stroke = 0.6) +
      scale_color_manual(values = status_palette) +
      scale_size(range = c(3, 10)) +
      coord_equal() +
      labs(x = "Longitude", y = "Latitude", color = "Status", size = "Wait (min)") +
      theme_minimal(base_family = "Atkinson Hyperlegible") +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.margin = margin(12, 12, 12, 12)
      )
  })
}

shinyApp(ui, server)
