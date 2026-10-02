mod_tasks_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page-header",
      div(
        h2("Tasks", class = "page-title"),
        p("Suivi des tâches en Kanban ou en tableau", class = "page-subtitle")
      ),
      div(class = "header-actions",
        actionButton(ns("view_kanban"), "Kanban", class = "btn-toggle active"),
        actionButton(ns("view_table"),  "Table",  class = "btn-toggle"),
        actionButton(ns("new_task"), tagList(icon("plus"), "New Task"),
                     class = "btn btn-primary btn-sm")
      )
    ),

    conditionalPanel(
      condition = sprintf("output['%s'] %% 2 == 1", ns("view_mode")),
      div(class = "card", DTOutput(ns("tasks_table")))
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] %% 2 == 0", ns("view_mode")),
      uiOutput(ns("kanban"))
    )
  )
}

mod_tasks_server <- function(id, actor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh  <- reactiveVal(0)
    # view_mode : entier (pair = kanban, impair = table)
    view_mode <- reactiveVal(0)

    observeEvent(input$view_kanban, view_mode(0))
    observeEvent(input$view_table,  view_mode(1))

    output$view_mode <- reactive({ view_mode() })
    outputOptions(output, "view_mode", suspendWhenHidden = FALSE)

    users_choices <- reactive({
      df <- get_users(actor()); setNames(as.character(df$id), df$full_name)
    })
    projects_choices <- reactive({
      df <- get_projects(actor()); setNames(as.character(df$id), df$name)
    })

    output$kanban <- renderUI({
      refresh()
      current_actor <- actor()
      df <- get_tasks(current_actor)
      status_choices <- c("todo", "in_progress", "review", "done")
      status_labels <- c(todo = "To do", in_progress = "In progress", review = "Review", done = "Done")

      status_control <- function(task_id, current_status, editable = TRUE) {
        if (!editable) {
          return(span(class = "kanban-readonly-status", status_labels[[as.character(current_status)]]))
        }
        tags$select(
          class = "kanban-status-select",
          title = "Changer le statut",
          onclick = "event.stopPropagation()",
          onchange = sprintf(
            "Shiny.setInputValue('%s', {id: %d, status: this.value}, {priority: 'event'}); event.stopPropagation();",
            ns("status_change"), as.integer(task_id)
          ),
          lapply(status_choices, function(status) {
            tags$option(
              value = status,
              selected = if (identical(status, as.character(current_status))) "selected" else NULL,
              status_labels[[status]]
            )
          })
        )
      }

      task_action <- function(input_id, task_id, label, icon_name, class_name = "") {
        tags$button(
          type = "button",
          class = paste("kanban-action", class_name),
          title = label,
          `aria-label` = label,
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'}); event.stopPropagation(); return false;",
            ns(input_id), as.integer(task_id)
          ),
          icon(icon_name)
        )
      }

      statuses <- list(
        todo        = list(label = "TO DO",       color = "#64748B"),
        in_progress = list(label = "IN PROGRESS", color = "#2563EB"),
        review      = list(label = "REVIEW",      color = "#F59E0B"),
        done        = list(label = "DONE",        color = "#16A34A")
      )
      cols <- lapply(names(statuses), function(st) {
        tasks <- df[df$status == st, , drop = FALSE]
        div(class = "kanban-col",
          div(class = "kanban-head",
            span(class = "kanban-dot", style = sprintf("background:%s", statuses[[st]]$color)),
            span(statuses[[st]]$label),
            span(class = "kanban-count", nrow(tasks))
          ),
          if (nrow(tasks) == 0) div(class = "kanban-empty", "—") else
            lapply(seq_len(nrow(tasks)), function(i) {
              task_id <- tasks$id[i]
              children <- get_subtasks(task_id, actor())
              can_edit_task <- role_can(current_actor, "write_tasks") &&
                (!identical(current_actor$role, "member") ||
                   (!is.na(tasks$assigned_to[i]) && as.integer(tasks$assigned_to[i]) == current_actor$user_id))
              card_actions <- list()
              if (can_edit_task) {
                card_actions <- c(card_actions, list(
                  task_action("edit_id", task_id, "Modifier", "pen"),
                  task_action("add_subtask_id", task_id, "Ajouter une sous-tache", "plus")
                ))
              }
              if (role_can(current_actor, "delete_tasks")) {
                card_actions <- c(card_actions, list(
                  task_action("delete_id", task_id, "Supprimer", "trash", "kanban-action-danger")
                ))
              }
              priority_class <- switch(
                as.character(tasks$priority[i]),
                high = "badge-danger",
                medium = "badge-warning",
                low = "badge-muted",
                "badge-muted"
              )
              div(
                class = if (can_edit_task) "kanban-card kanban-card-clickable" else "kanban-card",
                role = if (can_edit_task) "button" else NULL,
                tabindex = if (can_edit_task) "0" else NULL,
                title = if (can_edit_task) "Cliquer pour modifier cette tâche" else NULL,
                onclick = if (can_edit_task) sprintf(
                  "Shiny.setInputValue('%s', %d, {priority: 'event'});",
                  ns("edit_id"), as.integer(task_id)
                ) else NULL,
                div(class = "kanban-card-top",
                  div(class = "kanban-title", tasks$title[i]),
                  if (length(card_actions) > 0) div(class = "kanban-card-actions", card_actions)
                ),
                div(class = "kanban-meta",
                  span(class = paste("badge", priority_class), tasks$priority[i]),
                  span(class = "kanban-assignee", tasks$assignee_name[i])
                ),
                div(class = "kanban-card-footer",
                  status_control(task_id, tasks$status[i], can_edit_task),
                  if (!is.na(tasks$due_date[i]))
                    div(class = "kanban-date", icon("calendar"), format_date(tasks$due_date[i]))
                ),
                if (nrow(children) > 0) div(class = "kanban-subtasks",
                  div(class = "kanban-subtasks-heading",
                    icon("list-check"),
                    sprintf("Sous-taches (%d)", nrow(children))
                  ),
                  lapply(seq_len(nrow(children)), function(child_index) {
                    child <- children[child_index, , drop = FALSE]
                    can_edit_child <- role_can(current_actor, "write_tasks") &&
                      (!identical(current_actor$role, "member") ||
                         (!is.na(child$assigned_to[1]) && as.integer(child$assigned_to[1]) == current_actor$user_id))
                    div(class = "kanban-subtask-row",
                      span(class = "kanban-subtask-title", child$title),
                      status_control(child$id, child$status, can_edit_child),
                      if (role_can(current_actor, "delete_tasks"))
                        task_action("delete_id", child$id, "Supprimer la sous-tache", "trash", "kanban-action-danger")
                    )
                  })
                ) else div(class = "kanban-subtask-summary", icon("list-check"), "Aucune sous-tache")
              )
            })
          )
      })
      div(class = "kanban-board", cols)
    })

    output$tasks_table <- renderDT({
      refresh()
      df <- get_tasks(actor())
      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No tasks yet."), rownames = FALSE,
                         options = list(dom = 't')))
      }
      df_display <- data.frame(
        Title    = df$title,
        Project  = df$project_name %||% "—",
        Assignee = df$assignee_name,
        Status   = status_badge(df$status),
        Priority = priority_badge(df$priority),
        Due      = vapply(df$due_date, format_date, character(1)),
        Actions  = action_buttons(ns, df$id),
        stringsAsFactors = FALSE
      )
      datatable(df_display, escape = FALSE, rownames = FALSE,
                class = "stripe hover compact",
                options = list(dom = 'ft', pageLength = 12))
    }, server = TRUE)

    open_modal <- function(task = NULL) {
      is_edit <- !is.null(task)
      showModal(modalDialog(
        title = if (is_edit) "Edit task" else "New task",
        easyClose = TRUE, size = "m",
        textInput(ns("t_title"), "Title", value = if (is_edit) task$title else ""),
        textAreaInput(ns("t_desc"), "Description", rows = 2,
                      value = if (is_edit) task$description %||% "" else ""),
        fluidRow(
          column(6, selectInput(ns("t_project"), "Project",
            choices = c("—" = "", projects_choices()),
            selected = if (is_edit) task$project_id %||% "" else "")),
          column(6, selectInput(ns("t_assignee"), "Assignee",
            choices = c("—" = "", users_choices()),
            selected = if (is_edit) task$assigned_to %||% "" else ""))
        ),
        fluidRow(
          column(6, selectInput(ns("t_status"), "Status",
            choices = c("todo","in_progress","review","done"),
            selected = if (is_edit) task$status else "todo")),
          column(6, selectInput(ns("t_priority"), "Priority",
            choices = c("low","medium","high"),
            selected = if (is_edit) task$priority else "medium"))
        ),
        dateInput(ns("t_due"), "Due date",
                  value = if (is_edit) task$due_date %||% Sys.Date() else Sys.Date()),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_task"), if (is_edit) "Save" else "Create",
                       class = "btn btn-primary")
        )
      ))
      session$userData$editing_task_id <- if (is_edit) task$id else NULL
    }

    observeEvent(input$new_task, open_modal())
    observeEvent(input$edit_id, {
      df <- get_tasks(actor()); task <- df[df$id == input$edit_id, , drop = FALSE]
      if (nrow(task) == 1) open_modal(task)
    })

    observeEvent(input$status_change, {
      change <- input$status_change
      req(!is.null(change$id), change$status %in% c("todo", "in_progress", "review", "done"))
      tryCatch({
        update_task_status(as.integer(change$id), change$status, actor())
        refresh(refresh() + 1)
      }, error = function(e) {
        message("task status error: ", e$message)
        showNotification("Impossible de modifier le statut.", type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$add_subtask_id, {
      parent <- get_task(as.integer(input$add_subtask_id), actor())
      if (nrow(parent) != 1 || !is.na(parent$parent_task_id[1])) return()
      session$userData$subtask_parent <- parent[1, , drop = FALSE]
      showModal(modalDialog(
        title = paste("Nouvelle sous-tache pour", parent$title[1]),
        textInput(ns("subtask_title"), "Titre", placeholder = "Ex. Valider le formulaire"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("save_subtask"), "Ajouter", class = "btn btn-primary")
        )
      ))
    })

    observeEvent(input$save_subtask, {
      parent <- session$userData$subtask_parent
      title <- trimws(input$subtask_title %||% "")
      if (is.null(parent) || !nzchar(title)) {
        showNotification("Le titre de la sous-tache est obligatoire.", type = "error")
        return()
      }
      tryCatch({
        create_task(list(
          title = title,
          project_id = parent$project_id[1],
          assigned_to = parent$assigned_to[1],
          status = "todo",
          priority = parent$priority[1],
          due_date = NA_character_,
          parent_task_id = parent$id[1]
        ), actor())
        session$userData$subtask_parent <- NULL
        removeModal()
        refresh(refresh() + 1)
        showNotification("Sous-tache ajoutee.", type = "message")
      }, error = function(e) {
        message("subtask save error: ", e$message)
        showNotification("Impossible d'ajouter la sous-tache.", type = "error")
      })
    })

    observeEvent(input$save_task, {
      data <- list(
        title       = trimws(input$t_title),
        description = trimws(input$t_desc),
        project_id  = input$t_project,
        assigned_to = input$t_assignee,
        status      = input$t_status,
        priority    = input$t_priority,
        due_date    = as.character(input$t_due)
      )
      if (!nzchar(data$title)) {
        showNotification("Le titre est obligatoire.", type = "error"); return()
      }
      tryCatch({
        eid <- session$userData$editing_task_id
        if (is.null(eid)) {
          create_task(data, actor()); showNotification("Task created successfully.", type = "message")
        } else {
          update_task(eid, data, actor()); showNotification("Task updated successfully.", type = "message")
        }
        session$userData$editing_task_id <- NULL
        removeModal(); refresh(refresh() + 1)
      }, error = function(e) {
        message("task save error: ", e$message)
        showNotification("Unable to save the task.", type = "error")
      })
    })

    observeEvent(input$delete_id, {
      showModal(modalDialog(
        title = "Delete task",
        "Are you sure?",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_task"), "Delete", class = "btn btn-danger")
        )
      ))
      session$userData$deleting_task_id <- input$delete_id
    })

    observeEvent(input$confirm_delete_task, {
      id <- session$userData$deleting_task_id
      if (!is.null(id)) {
        tryCatch({
          delete_task(id, actor())
          showNotification("Task deleted.", type = "message")
          refresh(refresh() + 1)
        }, error = function(e) showNotification("Unable to delete task.", type = "error"))
      }
      session$userData$deleting_task_id <- NULL
      removeModal()
    })
  })
}