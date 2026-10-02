app_server <- function(input, output, session) {
  db_init()

  if (auth_setup_required()) {
    auth_setup_server(input, output, session)
    return(invisible(NULL))
  }

  authenticated_user <- shinymanager::secure_server(
    check_credentials = auth_credentials,
    timeout = 15
  )
 actor <- shiny::reactive({
    shiny::req(authenticated_user$user_id, authenticated_user$organization_id, authenticated_user$role)
    auth_actor(shiny::reactiveValuesToList(authenticated_user))
  })

  shiny::observe({
    current_actor <- actor()
    allowed_tabs <- c(
      dashboard = role_can(current_actor, "read_dashboard"),
      projects = role_can(current_actor, "read_projects"),
      tasks = role_can(current_actor, "read_tasks"),
      customers = role_can(current_actor, "read_customers"),
      activities = role_can(current_actor, "read_activities"),
      users = role_can(current_actor, "manage_users")
    )
    session$sendCustomMessage("startup-os-navigation", as.list(allowed_tabs))
  })

  mod_dashboard_server("dashboard", actor)
  mod_projects_server("projects", actor)
  mod_tasks_server("tasks", actor)
  mod_customers_server("customers", actor)
  mod_activities_server("activities", actor)
  mod_users_server("users", actor)
}