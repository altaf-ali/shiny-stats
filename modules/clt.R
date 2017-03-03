NUM_SAMPLES_MIN <- 5
NUM_SAMPLES_MAX <- 1000
NUM_SAMPLES_DEFAULT <- 5

clt_module <- list(
  name = "Central Limit Theorem",
  icon = icon("bar-chart")
)

clt_sidebar <- function(id) {
  list(
    sliderInput("sample_size",
                "Sample Size",
                min = NUM_SAMPLES_MIN,
                max = NUM_SAMPLES_MAX,
                value = NUM_SAMPLES_DEFAULT,
                animate = FALSE) # animationOptions(interval = 100))
  )
}

clt_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    h2("Central Limit Theorem"),
    fluidRow(
      #ggvisOutput("plot_clt")
      plotOutput("plot")
    )
  )
}

clt_server <- function(id, input, output, server) {
  set.seed(12345)
  diabetic_data <- read_csv("http://uclspp.github.io/PUBLG100/data/diabetic_data.csv")

  roll_dice <- function(n) {
    sample(1:6, n, replace=TRUE)
  }

  sample_data <- reactive({
    data_frame(x1=roll_dice(input$num_samples), x2=roll_dice(input$sample_size)) %>%
      mutate(roll_sum = x1+x2, roll_mean = (x1+x2)/2)
  })

  hospital_data <- reactive({
    samples <- replicate(1000, sample(diabetic_data$time_in_hospital, input$sample_size))
    sample_mean <- apply(samples, 2, mean)
    data_frame(time_in_hospital = sample_mean)
  })

  # reactive({
  #   hospital_data %>%
  #     ggvis(x = ~time_in_hospital) %>%
  #     layer_histograms() #%>%
  #     #scale_numeric("x", domain = c(1, 14))
  # }) %>%
  # bind_shiny("plot_clt")

  output$plot <- renderPlot({
    hist(hospital_data()$time_in_hospital, breaks = 14, main = paste("Sample Size = ", input$sample_size), xlab = "Mean")
  })
}


