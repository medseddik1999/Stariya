app_ui <- function() {
  shinydashboard::dashboardPage(
    skin = "blue",

    shinydashboard::dashboardHeader(
      titleWidth = 230,
      title = tags$div(class = "app-logo",
        tags$span(class = "logo-mark", "S"),
        tags$span(class = "logo-text", "Startup OS")
      ),
      tags$li(class = "dropdown header-search-li",
        tags$div(class = "header-search",
          shiny::icon("magnifying-glass"),
          tags$input(type = "text", placeholder = "Search anything")
        )
      ),
      tags$li(class = "dropdown",
        tags$a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
          shiny::icon("bell"), tags$span(class = "badge-dot")),
        tags$ul(class = "dropdown-menu dropdown-menu-right notif-menu",
          tags$li(class = "notif-head", "Notifications"),
          tags$li(tags$a(href = "#", "2 tâches arrivent à échéance")),
          tags$li(tags$a(href = "#", "Nouveau lead : Vandelay Industries"))
        )
      ),
      tags$li(class = "dropdown",
        tags$a(href = "#", class = "dropdown-toggle user-toggle", `data-toggle` = "dropdown",
          div(class = "user-avatar", "JD"),
          div(class = "user-summary",
            div(class = "user-name", "John Doe"),
            div(class = "user-mail", "john@acme.io")
          ),
          icon("chevron-down")
        ),
        tags$ul(class = "dropdown-menu dropdown-menu-right user-menu",
          tags$li(class = "user-info",
            div(class = "user-name", "John Doe"),
            div(class = "user-mail", "john@acme.io")),
          tags$li(class = "divider"),
          tags$li(tags$a(href = "#", shiny::icon("user"), " Profile")),
          tags$li(tags$a(href = "#", shiny::icon("gear"), " Settings")),
          tags$li(tags$a(href = "#", shiny::icon("right-from-bracket"), " Logout"))
        )
      )
    ),

    shinydashboard::dashboardSidebar(
      width = 230,
      shinydashboard::sidebarMenu(
        id = "sidebar_menu",
        tags$li(class = "header sidebar-section", "MENU"),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
        tags$li(class = "header sidebar-section", "WORKSPACE"),
        menuItem("Projects", tabName = "projects", icon = icon("folder")),
        menuItem("Tasks",    tabName = "tasks",    icon = icon("list-check")),
        tags$li(class = "header sidebar-section", "RELATIONSHIPS"),
        menuItem("Customers", tabName = "customers", icon = icon("users")),
        tags$li(class = "header sidebar-section", "HISTORY"),
        menuItem("Activity Center", tabName = "activities", icon = icon("bolt")),
        tags$li(class = "header sidebar-section", "ADMINISTRATION"),
        menuItem("Team", tabName = "users", icon = icon("user-shield"))
      )
    ),

    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$link(rel = "stylesheet", href = "css/custom.css?v=11"),
        tags$script(src = "js/custom.js")
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "dashboard",  mod_dashboard_ui("dashboard")),
        shinydashboard::tabItem(tabName = "projects",   mod_projects_ui("projects")),
        shinydashboard::tabItem(tabName = "tasks",      mod_tasks_ui("tasks")),
        shinydashboard::tabItem(tabName = "customers",  mod_customers_ui("customers")),
        shinydashboard::tabItem(tabName = "activities", mod_activities_ui("activities"))
        ,shinydashboard::tabItem(tabName = "users", mod_users_ui("users"))
      )
    )
  )
}

app_ui_root <- function(request) {
  if (auth_setup_required()) {
    return(auth_setup_ui())
  }

  protected_ui <- shinymanager::secure_app(
    app_ui(),
    language = "fr",
    head_auth = tags$head(tags$link(rel = "stylesheet", href = "css/custom.css?v=11"))
  )
  protected_ui(request)
}