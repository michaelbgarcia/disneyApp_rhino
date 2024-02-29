# app/main.R

box::use(
  shiny[reactive, observe, selectInput, updateSelectInput, bindEvent, moduleServer, NS, req],
  bslib[page_sidebar, sidebar, card, card_header],
  dplyr[filter],
  readr[read_rds]
)

box::use(
  app/view/map,
  app/view/table,
  app/logic/data_fetch[get_data_live]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    title = "Disney World Wait Times",
    sidebar = sidebar(
      selectInput(ns("park_select"), "Select Park", choices = NULL)
    ),
    card(
      card_header("Ride Detail"),
      table$ui(ns("table"))
    ),
    card(
      card_header("Ride Locations"),
      map$ui(ns("map"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    parks_data = read_rds("app/static/parks_data.rds")

    observe({
      updateSelectInput(session, "park_select", choices = unique(parks_data$parks_name))
    })

    live_data <- reactive({
      get_data_live(parks_data)
    })

    live_data_park <- reactive({
      req(input$park_select)
      live_data() |>
        filter(parks_name %in% input$park_select)
    }) |>
      bindEvent(input$park_select)

    map$server("map", data = live_data_park)
    table$server("table", data = live_data_park)
  })
}
