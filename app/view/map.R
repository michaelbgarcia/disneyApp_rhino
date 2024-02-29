# app/view/map.R

box::use(
  shiny[NS, moduleServer],
  leaflet[leaflet, addTiles, addCircleMarkers, leafletOutput, renderLeaflet]
)

box::use(
  app/logic/map_utils[data_labels],
)

#' @export
ui <- function(id){
  ns <- NS(id)
  leafletOutput(ns("map"))
}

#' @export
server <- function(id, data){
  moduleServer(id, function(input, output, session){
    output$map <- renderLeaflet({
      leaflet(data = data()) |>
        addTiles() |>
        addCircleMarkers(
          lat = ~latitude,
          lng = ~longitude,
          label = ~data_labels(name),
          color = ~status_cols,
          stroke = TRUE, fillOpacity = 0.75
        )
    })

  })
}
