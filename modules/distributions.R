NORM_MEAN_MIN = -10
NORM_MEAN_MAX = abs(NORM_MEAN_MIN)
NORM_MEAN_DEFAULT = NORM_MEAN_MIN + ((NORM_MEAN_MAX - NORM_MEAN_MIN) / 2)

NORM_SD_MIN = 0
NORM_SD_MAX = 10
NORM_SD_STEP = 0.1
NORM_SD_DEFAULT = 1

T_DF_MIN = 1
T_DF_MAX = 300
T_DF_DEFAULT = 5

PLOT_SAMP_MIN = 10
PLOT_SAMP_MAX = 1000
PLOT_SAMP_DEFAULT = 300

PLOT_POINT_MIN = 1
PLOT_POINT_MAX = 100
PLOT_POINT_DEFAULT = 5

distributions_module <- function(id) {
  list(
    name = "Distributions",
    icon = icon("area-chart")
  )
}

distributions_sidebar <- function(id) {
  list(
    h4("Normal Distribution:"),
    sliderInput("norm_mean", "Mean", min = NORM_MEAN_MIN, max = NORM_MEAN_MAX, value = NORM_MEAN_DEFAULT),
    sliderInput("norm_sd", "Standard Deviation", min = NORM_SD_MIN, max = NORM_SD_MAX, value = NORM_SD_DEFAULT, step = NORM_SD_STEP),
    h4("Student's t Distribution:"),
    sliderInput("t_df", "Degrees of Freedom", min = T_DF_MIN, max = T_DF_MAX, value = T_DF_DEFAULT),
    h4("Plot Options:"),
    sliderInput("plot_num_samples", "Number of Points", min = PLOT_SAMP_MIN, max = PLOT_SAMP_MAX, value = PLOT_SAMP_DEFAULT),
    sliderInput("plot_point_size", "Point Size", min = PLOT_POINT_MIN, max = PLOT_POINT_MAX, value = PLOT_POINT_DEFAULT)
  )
}

distributions_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    h2("Distributions"),
    fluidRow(
      box(width = 6, title = strong("Probability Density Function"), ggvisOutput("plot_pdf")),
      box(width = 6, title = strong("Cumulative Distribution Function"), ggvisOutput("plot_cdf"))
    )
  )
}

distributions_server <- function(id, input, output, server) {
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
  bind_shiny("plot_pdf")

  reactive({
    dist_data %>%
      ggvis(x = ~x, y = ~cdf) %>%
      add_axis("x", title = "") %>%
      add_axis("y", title = "") %>%
      layer_points(size := input$plot_point_size, stroke = ~Distribution, fill = ~Distribution)
  }) %>%
    bind_shiny("plot_cdf")

}
