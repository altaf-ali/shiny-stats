demos_module <- list(
  name = "Demos",
  icon = icon("cubes"),
  subitems = c(
    "distributions",
    "ols"
  )
)

demos_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    h3(demos_module$name)
  )
}
