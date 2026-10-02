# Startup OS - entry point
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjs)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(DBI)
  library(RSQLite)
  library(pool)
})

source("R/config.R")
source("R/database.R")
source("R/db_helpers.R")
source("R/helpers.R")
source("R/auth.R")

source("modules/mod_dashboard.R")
source("modules/mod_projects.R")
source("modules/mod_tasks.R")
source("modules/mod_customers.R")
source("modules/mod_activities.R")
source("modules/mod_users.R")

source("R/ui.R")
source("R/server.R")

shiny::shinyApp(ui = app_ui_root, server = app_server)