# app/main.R

box::use(
  shiny[...],
  shinyjs[...],
  bslib[page_sidebar, sidebar, card, card_header],
  dplyr[filter, mutate],
  readr[read_rds],
)

box::use(
  app/view/map,
  app/view/table,
  app/logic/data_fetch[get_data_live],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css")
    ),
    useShinyjs(),
    extendShinyjs("static/shinyjs.js",functions = c("backgroundCol")),
    title = "Disney World Wait Times",
    sidebar = sidebar(
      selectInput(ns("park_select"), "Select Park", choices = NULL),
      selectInput(ns("col"), "Colour",
                  c("green", "yellow", "red", "blue", "white")),
      selectInput(ns("selector"), "Element", c("sport", "name", "button")),
      p(id = ns("name"), "My name is Dean"),
      p(id = ns("sport"), "I like soccer"),
      actionButton(ns("button"), "Go")
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
    ns <- session$ns
    observe({
      updateSelectInput(session, "park_select", choices = unique(parks_data$parks_name))
    })

    observeEvent(input$button, {
      js$backgroundCol(input$selector, input$col, ns(""))
    })

    live_data <- reactive({
      get_data_live(parks_data)
    })

    live_data_park <- reactive({
      req(input$park_select)
      live_data() |>
        filter(parks_name %in% input$park_select) |>
        mutate(Details = id)
    }) |>
      bindEvent(input$park_select)

    map$server("map", data = live_data_park)
    modalVal = table$server("table", data = live_data_park)

    observe({
      showModal(modalDialog(
        title = "Modal Title",
        paste("You clicked on:", modalVal())
      ))
    })

  })
}
