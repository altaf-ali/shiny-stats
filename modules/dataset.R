library(DT)
library(tools)

# -----------------------------------------------------------------------------
dataset_load_auto <- function(dataset) {
  format <- dataset$formats %>%
    filter(type == toupper(tools::file_ext(dataset$path))) %>%
    as.list()

  format$handler[[1]](dataset)
}

# -----------------------------------------------------------------------------
dataset_load <- function(dataset) {
  dataset$format$handler[[1]](dataset)
}

# -----------------------------------------------------------------------------
dataset_load_installed <- function(dataset) {
  is_datasets_package <- (dataset$installed$Package == "datasets")
  package <- ifelse(is_datasets_package, NA, as.character(dataset$installed$Package))

  if (!is.na(package))
    do.call(library, list(package))

  dataset_name <- as.character(dataset$installed$Item)
  do.call(data, list(dataset_name))
  get(dataset_name)
}

# -----------------------------------------------------------------------------
dataset_load_csv <- function(dataset) {
  read.csv(dataset$path)
}

# -----------------------------------------------------------------------------
dataset_load_tab <- function(dataset) {
  read.table(dataset$path)
}

# -----------------------------------------------------------------------------
dataset_load_dta <- function(dataset) {
  foreign::read.dta(dataset$path)
}

# -----------------------------------------------------------------------------
dataset_module <- list(
  name = "Dataset",
  dynamic = TRUE,
  icon = icon("database"),

  defaults = list(
    url = "http://uclspp.github.io/PUBLG100/data/caschool.dta"
  ),

  file_formats = frame_data(
    ~type, ~description, ~handler,
    "AUTO", "Auto-Detect", dataset_load_auto,
    "CSV", "Command Separated Values", dataset_load_csv,
    "DTA", "Stata", dataset_load_dta,
    "TAB", "Tab Delimited", dataset_load_tab
  ),

  source_handlers = list(
    "URL" = dataset_load,
    "Local" = dataset_load,
    "All Available" = dataset_load_installed
  ),

  datasets = data.frame(data(package = .packages(all.available = TRUE))$results)
)

dataset_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    h3(dataset_module$name),
    fluidRow(
      tabBox(
        id = ns("source"),
        width = 12,
        tabPanel(names(dataset_module$source_handlers)[1],
                 textInput(ns("url"),
                           "Internet Address",
                           value = dataset_module$defaults$url,
                           width = "100%")
                 ),
        tabPanel(names(dataset_module$source_handlers)[2],
                 fileInput(ns('filename'),
                           'Local File',
                           accept = dataset_module$file_types)
                 ),
        tabPanel(names(dataset_module$source_handlers)[3], DT::dataTableOutput(ns("installed_datasets"))),
        br(),
        conditionalPanel(
          condition = sprintf("input['%s'] != '%s'", ns("source"), names(dataset_module$source_handlers)[3]),
          radioButtons(ns("format"),
                       label = "Format",
                       choices = setNames(row.names(dataset_module$file_formats), dataset_module$file_formats$description))
        ),
        conditionalPanel(
          condition = 1,
          actionButton(ns("load_data"), label = "Load Data")
        )
      )
    ),
    fluidRow(
      uiOutput(ns("struct_left")),
      uiOutput(ns("struct_right"))
    )
  )
}

dataset_server <- function(input, output, session, id, data) {
  ns <- NS(id)

  v <- reactiveValues(
    dataset = NULL,
    selected_var = 0
  )

  get_data_struct <- reactive({
    data_struct <- data_frame(
      Class = character(),
      Min = numeric(),
      Median = numeric(),
      Mean = numeric(),
      Max = numeric(),
      `NA` = numeric()
    )

    for (var in names(v$dataset)) {
      data_struct[var, "Class"] = class(v$dataset[,var])

      if (is.numeric(v$dataset[,var])) {
        data_struct[var, "Min"] <- min(v$dataset[,var], na.rm = TRUE)
        data_struct[var, "Max"] <- max(v$dataset[,var], na.rm = TRUE)
        data_struct[var, "Mean"] <- mean(v$dataset[,var], na.rm = TRUE)
        data_struct[var, "Median"] <- median(v$dataset[,var], na.rm = TRUE)
      }

      data_struct[var, "NA"] <- sum(is.na(v$dataset[,var]))
    }

    return(data_struct)
  })

  output$dataset <- renderMenu({
    badge <- list(
      color = "red",
      label = "none"
    )

    if (is.data.frame(v$dataset)) {
      if (nrow(v$dataset) > 0 && ncol(v$dataset) > 0) {
        badge$color <- "green"
        badge$label <- nrow(v$dataset)
      }
    } else {
      badge$label <- "invalid"
    }

    menuItem(dataset_module$name, tabName = id, icon = dataset_module$icon, badgeLabel = badge$label, badgeColor = badge$color)
  })

  output$installed_datasets <- DT::renderDataTable({
    DT::datatable(
      dplyr::select(data.frame(dataset_module$datasets), Package, Item, Title),
      selection = "single",
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 25, 50, 100)
      )
    )
  })

  observeEvent(input$load_data, {
    v$dataset <- dataset_module$source_handlers[[input$source]](list(
      path = if_else(input$source == "URL", input$url, input$filename),
      installed = as.list(dataset_module$datasets[input$installed_datasets_rows_selected, ]),
      format = as.list(dataset_module$file_formats[as.integer(input$format), ]),
      formats = dataset_module$file_formats
    ))

    if (!is.data.frame(v$dataset))
      v$dataset <- NULL
  })

  output$struct_left <- renderUI({
    if (is.null(v$dataset))
      return()

    column(width = 7, DT::dataTableOutput(ns("struct_table")))
  })

  output$struct_right <- renderUI({
    if (is.null(v$dataset))
      return()

    column(width = 5,
        ggvisOutput(ns("struct_plot")),
        DT::dataTableOutput(ns("struct_detail"))
    )
  })

  observeEvent(input$struct_table_rows_selected, {
    v$selected_var <- input$struct_table_rows_selected
  })

  output$struct_table <- DT::renderDataTable({
    data_struct <- get_data_struct()

    if (!nrow(data_struct))
      return()

    DT::datatable(data_struct, selection = 'single') %>%
      formatRound(3:ncol(data_struct)-1, 2) %>%
      formatRound(ncol(data_struct), 0)
  })

  reactive({
    data <- data_frame(values = numeric())

    height <- 0
    var_name <- ""

    if (is.numeric(v$dataset[,v$selected_var])) {
      height <- NULL
      var_name <- names(v$dataset)[v$selected_var]
      data <- select(v$dataset, values = v$selected_var)
    }

    data %>%
      ggvis(~values, fill := "#bd0026") %>%
      set_options(height = height, resizable = FALSE) %>%
      layer_histograms() %>%
      add_axis("x", title = var_name, subdivide = 1) %>%
      add_axis("y", title = "")
  }) %>%
  bind_shiny(ns("struct_plot"))

  output$struct_summaryX <- renderText({
    if (is.null(v$dataset))
      return()

    data <- v$dataset[,v$selected_var]

    if (is.character(data)) {
      data <- as.factor(data)
    }

    if (is.factor(data)) {
      paste(levels(data), collapse='<br/>')
    }
  })

  output$struct_detail <- DT::renderDataTable({
    if (is.null(v$dataset) || !v$selected_var)
      return()

    if (is.numeric(v$dataset[,v$selected_var]))
      return()

    data.frame(Value = v$dataset[,v$selected_var]) %>%
      group_by(Value) %>%
      summarize(Count = n()) %>%
      DT::datatable(selection = 'single',
                    rownames = FALSE,
                    options = list(scrollY = "370px", scrollCollapse = TRUE, paging = FALSE))
  })

  return(reactive(v$dataset))
}
