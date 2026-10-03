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
    shiny::req(
      authenticated_user$user_id,
      authenticated_user$organization_id,
      authenticated_user$role
    )
    auth_actor(shiny::reactiveValuesToList(authenticated_user))
  })

  # ============================================================
  # HEADER — Données utilisateur (reactive partagée)
  # ============================================================
  user_display <- shiny::reactive({
    a     <- actor()
    first <- trimws(a$first_name %||% "")
    last  <- trimws(a$last_name  %||% "")
    email <- a$email %||% ""

    full_name <- trimws(paste(first, last))
    has_name  <- nzchar(full_name)
    primary   <- if (has_name) full_name else email

    initials <- if (has_name) {
      paste0(toupper(substr(first, 1, 1)), toupper(substr(last, 1, 1)))
    } else {
      local_part <- sub("@.*$", "", email)
      toupper(substr(local_part, 1, 2))
    }
    if (!nzchar(initials)) initials <- "?"

    list(primary = primary, email = email, initials = initials, has_name = has_name)
  })

  output$header_initials     <- shiny::renderText({ user_display()$initials })
  output$header_primary_top  <- shiny::renderText({ user_display()$primary })
  output$header_primary_menu <- shiny::renderText({ user_display()$primary })

  # On n'affiche l'email en dessous QUE si un nom est déjà affiché au-dessus
  output$header_email_top <- shiny::renderText({
    d <- user_display()
    if (d$has_name) d$email else ""
  })
  output$header_email_menu <- shiny::renderText({
    d <- user_display()
    if (d$has_name) d$email else ""
  })

  # ============================================================
  # HEADER — Notifications dynamiques
  # ============================================================
  output$header_notif_badge <- shiny::renderUI({
    a <- actor()
    if (count_unread_notifications(a) > 0) {
      tags$span(class = "badge-dot")
    } else {
      NULL
    }
  })

  output$header_notif_items <- shiny::renderUI({
    a      <- actor()
    notifs <- get_notifications(a, limit = 5)
    unread <- count_unread_notifications(a)

    tagList(
      tags$li(
        class = "notif-head",
        sprintf("Notifications%s", if (unread > 0) sprintf(" (%d)", unread) else "")
      ),
      if (nrow(notifs) == 0) {
        tags$li(
          tags$a(
            href = "#",
            style = "color: var(--muted); font-size: 12px;",
            "Aucune notification"
          )
        )
      } else {
        lapply(seq_len(nrow(notifs)), function(i) {
          tags$li(
            tags$a(
              href = "#",
              tags$div(
                style = "font-weight: 600; font-size: 12px;",
                notifs$title[i]
              ),
              tags$div(
                style = "font-size: 11px; color: var(--muted); margin-top: 2px;",
                notifs$message[i]
              ),
              tags$div(
                style = "font-size: 10px; color: #b0b0bb; margin-top: 4px;",
                notifs$created_at[i]
              )
            )
          )
        })
      }
    )
  })

  # ============================================================
  # HEADER — Logout
  # ============================================================
  shiny::observeEvent(input$logout_trigger, {
    session$reload()
  }, ignoreInit = TRUE)

  # ============================================================
  # Navigation selon permissions
  # ============================================================
  shiny::observe({
    current_actor <- actor()
    allowed_tabs <- c(
      dashboard  = role_can(current_actor, "read_dashboard"),
      projects   = role_can(current_actor, "read_projects"),
      tasks      = role_can(current_actor, "read_tasks"),
      customers  = role_can(current_actor, "read_customers"),
      activities = role_can(current_actor, "read_activities"),
      users      = role_can(current_actor, "manage_users")
    )
    session$sendCustomMessage("startup-os-navigation", as.list(allowed_tabs))
  })

  # ============================================================
  # Modules
  # ============================================================
  mod_dashboard_server("dashboard", actor)
  mod_projects_server("projects", actor)
  mod_tasks_server("tasks", actor)
  mod_customers_server("customers", actor)
  mod_activities_server("activities", actor)
  mod_users_server("users", actor)
}