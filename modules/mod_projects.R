mod_projects_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page-header",
      div(
        h2("Projects", class = "page-title"),
        p("Gérez tous vos projets produits", class = "page-subtitle")
      ),
      actionButton(ns("new_project"), tagList(icon("plus"), "New Project"),
                   class = "btn btn-primary btn-sm")
    ),
    div(class = "card",
      DTOutput(ns("projects_table"))
    )
  )
}

mod_projects_server <- function(id, actor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    users_choices <- reactive({
      df <- get_users(actor())
      setNames(as.character(df$id), df$full_name)
    })

    output$projects_table <- renderDT({
      refresh()
      df <- get_projects(actor())
      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No projects yet."),
                         rownames = FALSE, options = list(dom = 't')))
      }
      df_display <- data.frame(
        Project  = df$name,
        Owner    = df$owner_name,
        Status   = status_badge(df$status),
        Priority = priority_badge(df$priority),
        Progress = sprintf('<div class="mini-progress"><div style="width:%d%%"></div></div>%d%%',
                           df$progress, df$progress),
        Deadline = vapply(df$end_date, format_date, character(1)),
        Actions  = action_buttons(ns, df$id),
        stringsAsFactors = FALSE
      )
      datatable(
        df_display,
        escape = FALSE,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          dom = 'ft',
          pageLength = 10,
          columnDefs = list(
            list(className = 'dt-left',   targets = 0),
            list(className = 'dt-center', targets = c(2,3,4))
          )
        )
      )
    }, server = TRUE)

    open_modal <- function(project = NULL) {
      is_edit <- !is.null(project)
      showModal(modalDialog(
        title = if (is_edit) "Edit project" else "New project",
        easyClose = TRUE,
        size = "m",
        textInput(ns("p_name"), "Name", value = if (is_edit) project$name else ""),
        textAreaInput(ns("p_desc"), "Description", rows = 3,
                      value = if (is_edit) project$description %||% "" else ""),
        fluidRow(
          column(6, selectInput(ns("p_status"), "Status",
            choices = c("planning","in_progress","review","done","on_hold"),
            selected = if (is_edit) project$status else "planning")),
          column(6, selectInput(ns("p_priority"), "Priority",
            choices = c("low","medium","high"),
            selected = if (is_edit) project$priority else "medium"))
        ),
        fluidRow(
          column(6, selectInput(ns("p_owner"), "Owner",
            choices = c("—" = "", users_choices()),
            selected = if (is_edit) project$owner_id %||% "" else "")),
          column(6, numericInput(ns("p_progress"), "Progress (%)",
            value = if (is_edit) project$progress else 0, min = 0, max = 100))
        ),
        fluidRow(
          column(6, dateInput(ns("p_start"), "Start date",
            value = if (is_edit) project$start_date %||% Sys.Date() else Sys.Date())),
          column(6, dateInput(ns("p_end"), "End date",
            value = if (is_edit) project$end_date %||% Sys.Date() else Sys.Date()))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_project"),
                       if (is_edit) "Save" else "Create",
                       class = "btn btn-primary")
        )
      ))
      # stocke l'id en édition
      session$userData$editing_project_id <- if (is_edit) project$id else NULL
    }

    observeEvent(input$new_project, { open_modal() })

    observeEvent(input$edit_id, {
      project <- get_project(input$edit_id, actor())
      if (nrow(project) == 1) open_modal(project)
    })

    observeEvent(input$save_project, {
      data <- list(
        name        = trimws(input$p_name),
        description = trimws(input$p_desc),
        status      = input$p_status,
        priority    = input$p_priority,
        owner_id    = input$p_owner,
        progress    = input$p_progress,
        start_date  = as.character(input$p_start),
        end_date    = as.character(input$p_end)
      )
      if (!nzchar(data$name)) {
        showNotification("Le nom est obligatoire.", type = "error")
        return()
      }
      tryCatch({
        eid <- session$userData$editing_project_id
        if (is.null(eid)) {
          create_project(data, actor())
          showNotification("Project created successfully.", type = "message")
        } else {
          update_project(eid, data, actor())
          showNotification("Project updated successfully.", type = "message")
        }
        session$userData$editing_project_id <- NULL
        removeModal()
        refresh(refresh() + 1)
      }, error = function(e) {
        message("project save error: ", e$message)
        showNotification("Unable to save the project. Please check required fields.", type = "error")
      })
    })

    observeEvent(input$delete_id, {
      id <- input$delete_id
      showModal(modalDialog(
        title = "Delete project",
        "Are you sure you want to delete this project?",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete", class = "btn btn-danger")
        )
      ))
      session$userData$deleting_project_id <- id
    })

    observeEvent(input$confirm_delete, {
      id <- session$userData$deleting_project_id
      if (!is.null(id)) {
        tryCatch({
          delete_project(id, actor())
          showNotification("Project deleted.", type = "message")
          refresh(refresh() + 1)
        }, error = function(e) {
          showNotification("Unable to delete project.", type = "error")
        })
      }
      session$userData$deleting_project_id <- NULL
      removeModal()
    })
  })
}