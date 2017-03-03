
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)
library(shinydashboard)

library(ggplot2)
library(ggvis)

library(readr)
library(dplyr)
library(tidyr)

library(RColorBrewer)

rm(list = ls())

source("dashboard.R")
source("config.R")

dashboard_sourceModules("modules")

dashboard_header <- dashboardHeader(title = App$title)

dashboard_sidebar <- dashboardSidebar(
  dashboard_sidebarMenu(App$modules)
)

dashboard_body <- dashboardBody(
  dashboard_includeCSS("main.css"),
  dashboard_tabItems(App$modules)
)

shinyApp(ui = dashboardPage(dashboard_header, dashboard_sidebar, dashboard_body), server = function(input, output, session) {
  dashboard_server(App$modules, input, output, session)
})


