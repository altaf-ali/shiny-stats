
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

library(ggvis)
library(readr)
library(dplyr)

source("dashboard.R")
source("config.R")

dashboard_sourceModules(App$modules)

dashboard_header <- dashboardHeader(title = App$title)

dashboard_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar_menu",
    dashboard_menuItems(App$modules),
    dashboard_menuItemsConditional(App$modules)
  )
)

dashboard_body <- dashboardBody(
  dashboard_tabItems(App$modules)
)

shinyApp(ui = dashboardPage(dashboard_header, dashboard_sidebar, dashboard_body), server = function(input, output, session) {
  dashboard_server(App$modules, input, output, session)
})


