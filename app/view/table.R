# app/view/table.R

box::use(
  shiny[NS, moduleServer, reactive, actionButton, icon, req],
  reactable[reactable, reactableOutput, renderReactable, colDef],
  reactablefmtr[fivethirtyeight, merge_column, pill_buttons]
)

#' @export
ui <- function(id){
  ns <- NS(id)
  reactableOutput(ns("table"))
}

#' @export
server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$table <- renderReactable({
      reactable::reactable(
        data = data(),
        theme = fivethirtyeight(centered = TRUE, header_font_size = 11),
        pagination = TRUE,
        showSortIcon = FALSE,
        highlight = TRUE,
        compact = TRUE,
        selection = "single",
        onClick = "expand",
        # defaultSelected = 1,
        columns = list(
          Details =
            colDef(
              cell = function(value, index) {
                actionButton(inputId = ns(paste("btn", index, sep = "_")),
                             label = "",
                             icon = icon("exclamation"),
                             onclick = sprintf(
                               "Shiny.setInputValue('%s', '%s');",
                               ns("selectedValue"),
                               value
                             )
                )
              },
              html = TRUE),
          id = colDef(show = FALSE),
          parks_name = colDef(show = FALSE),
          latitude = colDef(show = FALSE),
          longitude = colDef(show = FALSE),
          name = colDef(
            align = "left",
            cell = merge_column(data(), "parks_name", merged_position = "below"),
          ),
          status = colDef(
            maxWidth = 150,
            name = "Status",
            align = "center",
            cell = pill_buttons(data(), color_ref = "status_cols", opacity = 0.7),
          ),
          status_cols = colDef(show = FALSE),
          wait_time = colDef(
            maxWidth = 50,
            name = "Wait (min.)",
            align = "center"
          ),
          ll_time = colDef(
            name = "Next LL Time"
          ),
          lastUpdated = colDef(
            name = "Last Updated"
          )
        )
      )
    })


    return(reactive({
      req(input$selectedValue)
      input$selectedValue
    }))
  })
}
