distributions_module <- list(
  name = "Distributions",
  icon = icon("area-chart"),

  defaults = list(
    NORM_MEAN_MIN = -10,
    NORM_MEAN_MAX = 10,
    NORM_MEAN_DEFAULT = 0,

    NORM_SD_MIN = 0,
    NORM_SD_MAX = 10,
    NORM_SD_STEP = 0.1,
    NORM_SD_DEFAULT = 1,

    T_DF_MIN = 1,
    T_DF_MAX = 300,
    T_DF_DEFAULT = 5,

    PLOT_SAMP_MIN = 10,
    PLOT_SAMP_MAX = 1000,
    PLOT_SAMP_DEFAULT = 300,

    PLOT_POINT_MIN = 1,
    PLOT_POINT_MAX = 100,
    PLOT_POINT_DEFAULT = 5
  )
)

distributions_sidebar <- function(id) {
  ns <- NS(id)
  defaults <- distributions_module$defaults

  list(
    div(
      class="sidebar-well",
      h5(strong("Normal Distribution:")),
      sliderInput(ns("norm_mean"),
                  "Mean",
                  min = defaults$NORM_MEAN_MIN,
                  max = defaults$NORM_MEAN_MAX,
                  value = defaults$NORM_MEAN_DEFAULT),
      sliderInput(ns("norm_sd"),
                  "Standard Deviation",
                  min = defaults$NORM_SD_MIN,
                  max = defaults$NORM_SD_MAX,
                  value = defaults$NORM_SD_DEFAULT,
                  step = defaults$NORM_SD_STEP),
      h5(strong("Student's t Distribution:")),
      sliderInput(ns("t_df"),
                  "Degrees of Freedom",
                  min = defaults$T_DF_MIN,
                  max = defaults$T_DF_MAX,
                  value = defaults$T_DF_DEFAULT),
      hr(),
      sliderInput(ns("plot_num_samples"),
                  "Number of Points",
                  min = defaults$PLOT_SAMP_MIN,
                  max = defaults$PLOT_SAMP_MAX,
                  value = defaults$PLOT_SAMP_DEFAULT),
      sliderInput(ns("plot_point_size"),
                  "Point Size",
                  min = defaults$PLOT_POINT_MIN,
                  max = defaults$PLOT_POINT_MAX,
                  value = defaults$PLOT_POINT_DEFAULT)
    )
  )
}

distributions_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    h3(distributions_module$name),
    fluidRow(
      box(width = 6, title = strong("Probability Density Function"), ggvisOutput(ns("plot_pdf"))),
      box(width = 6, title = strong("Cumulative Distribution Function"), ggvisOutput(ns("plot_cdf")))
    )
  )
}

distributions_server <- function(input, output, session, id, data) {
  ns <- NS(id)

  dist_data <- reactive({
    x_min <- input$norm_mean - (input$norm_sd * 4)
    x_max <- abs(x_min)

    x_val <- seq(x_min, x_max, length = input$plot_num_samples)

    dist <- bind_rows(
      data_frame(
        x = x_val,
        dist = "Normal",
        pdf = dnorm(x),
        cdf = pnorm(x)
      ),
      data_frame(
        x = x_val,
        dist = "t-Distribution",
        pdf = dt(x, df = input$t_df),
        cdf = pt(x, df = input$t_df)
      )
    ) %>%
    mutate(dist = as.factor(dist)) %>%
    mutate(Distribution = dist)
  })

  reactive({
    dist_data %>%
      ggvis(x = ~x, y = ~pdf) %>%
      add_axis("x", title = "") %>%
      add_axis("y", title = "") %>%
      layer_points(size := input$plot_point_size, stroke = ~Distribution, fill = ~Distribution)
  }) %>%
  bind_shiny(ns("plot_pdf"))

  reactive({
    dist_data %>%
      ggvis(x = ~x, y = ~cdf) %>%
      add_axis("x", title = "") %>%
      add_axis("y", title = "") %>%
      layer_points(size := input$plot_point_size, stroke = ~Distribution, fill = ~Distribution)
  }) %>%
  bind_shiny(ns("plot_cdf"))
}
