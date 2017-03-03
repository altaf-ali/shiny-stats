table_module <- list(
  name = "Table",
  icon = icon("table")
)

table_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    h3(table_module$name),
    fluidRow(
      box(width = 12,
          status = "primary",
          DT::dataTableOutput(ns("table"))
      )
    )
  )
}

table_server <- function(input, output, session, id, data) {
  output$table <- renderDataTable({
    dataset <- data()

    if (!is.data.frame(dataset))
      return()

    DT::datatable(
      dataset,
      selection = "single",
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100)
      )
    )
  })

  return(data)
}
