correlations_module <- list(
  name = "Correlations",
  icon = icon("th-large")
)

correlations_sidebar <- function(id) {
  ns <- NS(id)

  list(
    sliderInput(ns("exclude"), label = "Exclude", min = -1, max = 1, step = 0.01, value = c(0, 0))
  )
}

correlations_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    h3(correlations_module$name),
    fluidRow(
      box(width = 12,
          status = "primary",
          selectizeInput(ns("variables"), "Variables", choices = c(), multiple = TRUE, width = "100%"),
          actionButton(ns("select_all"), "Select All"),
          hr(),
          plotOutput(ns("matrix"), width = "100%", height = "600px")
      )
    )
  )
}

correlations_server <- function(input, output, session, id, data) {
  get_dataset <- function() {
    dataset <- data()

    if (!is.data.frame(dataset))
      return()

    dataset %>%
      select_if(is.numeric)
  }

  select_all <- function(dataset) {
    if (!is.data.frame(dataset))
      return()

    variables <- names(dataset)
    updateSelectizeInput(session,
                         "variables",
                         choices = variables,
                         selected = variables)
  }

  observeEvent(data(), { select_all(get_dataset()) })

  observeEvent(input$select_all, { select_all(get_dataset()) })

  output$matrix <- renderPlot({
    dataset <- get_dataset()

    if (!is.data.frame(dataset))
      return()

    all_vars <- names(dataset)
    selected_vars <- all_vars[all_vars %in% input$variables]

    if (length(selected_vars) < 2)
      return()

    cor_matrix <- data.frame(cor(dataset, use = "pairwise.complete.obs"))

    cor_matrix <- cor_matrix[, selected_vars]
    cor_matrix$var1 <- row.names(cor_matrix)

    cor_matrix <- cor_matrix %>%
      filter(var1 %in% selected_vars)

    #cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA

    cor_matrix <- cor_matrix %>%
      gather(var2, value, 1:(ncol(cor_matrix)-1)) %>%
      mutate(var1 = factor(var1, levels = selected_vars, labels = selected_vars)) %>%
      mutate(var2 = factor(var2, levels = selected_vars, labels = selected_vars)) %>%
      filter(value < input$exclude[1] | value > input$exclude[2]) %>%
      na.omit()

    color_palette <- brewer.pal(n = 8, name = "RdBu")

    ggplot(data = cor_matrix, aes(var2, var1, fill = value)) +
      geom_tile() +
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "lightgray"),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 16),
            axis.text.y  = element_text(vjust = 0.5, size = 16)) +
      scale_fill_gradient2(low = first(color_palette),
                           high = last(color_palette),
                           limit = c(-1, 1),
                           space = "Lab",
                           name = "") +
      guides(fill = guide_colorbar(barwidth = 1, barheight = 16, title.theme = element_text(angle = 0, size = 14))) +
      geom_text(aes(var2, var1, label = round(value, digits=2)), color = "black", size = 4)
  })
}
