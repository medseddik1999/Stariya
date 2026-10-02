mod_users_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page-header",
      div(
        h2("Team", class = "page-title"),
        p("Comptes, rôles et accès de l’organisation", class = "page-subtitle")
      ),
      actionButton(ns("new_user"), tagList(icon("plus"), "Ajouter un utilisateur"),
                   class = "btn btn-primary btn-sm")
    ),
    div(class = "card", DTOutput(ns("users_table")))
  )
}

mod_users_server <- function(id, actor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    output$users_table <- renderDT({
      refresh()
      current_actor <- actor()
      if (!role_can(current_actor, "manage_users")) {
        return(datatable(data.frame(Message = "Accès non autorisé."), rownames = FALSE,
                         options = list(dom = "t")))
      }
      users <- admin_list_users(current_actor)
      if (nrow(users) == 0L) {
        return(datatable(data.frame(Message = "Aucun utilisateur."), rownames = FALSE,
                         options = list(dom = "t")))
      }
      actions <- vapply(seq_len(nrow(users)), function(i) {
        sprintf(
          '<div class="action-buttons"><button class="btn-action" title="Modifier le rôle" onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'})"><i class="fa fa-pen"></i></button></div>',
          ns("edit_user_id"), users$id[i]
        )
      }, character(1))
      display <- data.frame(
        Name = paste(users$first_name, users$last_name),
        Email = users$email,
        Organization = users$organization_name,
        `Organization ID` = users$organization_id,
        Role = users$role,
        Status = users$status,
        `Last login` = ifelse(is.na(users$last_login_at), "Never", users$last_login_at),
        Actions = actions,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      datatable(display, escape = FALSE, rownames = FALSE,
                class = "stripe hover compact",
                options = list(dom = "ft", pageLength = 12, autoWidth = TRUE))
    }, server = TRUE)

    role_choices <- function(current_actor, selected = "member") {
      available_roles <- if (identical(current_actor$role, "super_admin")) {
        auth_roles
      } else {
        c("admin", "member", "client")
      }
      selectInput(ns("user_role"), "Rôle", choices = available_roles, selected = selected)
    }

    open_user_modal <- function(current_actor, user = NULL) {
      is_edit <- !is.null(user)
      organizations <- admin_list_organizations(current_actor)
      org_choices <- setNames(as.character(organizations$id), organizations$name)
      fields <- list(
        textInput(ns("user_first_name"), "Prénom", value = if (is_edit) user$first_name else ""),
        textInput(ns("user_last_name"), "Nom", value = if (is_edit) user$last_name else ""),
        textInput(ns("user_email"), "Adresse e-mail", value = if (is_edit) user$email else ""),
        role_choices(current_actor, if (is_edit) user$role else "member")
      )
      if (identical(current_actor$role, "super_admin")) {
        fields <- append(fields, list(
          selectInput(ns("user_organization_id"), "Organisation existante",
                      choices = c("—" = "", org_choices),
                      selected = if (is_edit) as.character(user$organization_id) else ""),
          textInput(ns("user_organization_name"), "Ou créer une organisation", value = "")
        ))
      }
      if (is_edit) {
        fields <- append(fields, list(
          selectInput(ns("user_status"), "Statut", choices = c("active", "disabled"), selected = user$status),
          passwordInput(ns("user_new_password"), "Nouveau mot de passe (facultatif, 12 caractères minimum)"),
          passwordInput(ns("user_new_password_confirmation"), "Confirmer le nouveau mot de passe"),
          hidden(numericInput(ns("user_id"), NULL, value = user$id, min = 1))
        ))
      } else {
        fields <- append(fields, list(
          passwordInput(ns("user_password"), "Mot de passe temporaire (12 caractères minimum)"),
          passwordInput(ns("user_password_confirmation"), "Confirmer le mot de passe")
        ))
      }
      showModal(modalDialog(
        title = if (is_edit) "Modifier un compte" else "Ajouter un utilisateur",
        easyClose = TRUE,
        size = "m",
        do.call(tagList, fields),
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns("save_user"), if (is_edit) "Enregistrer" else "Créer le compte",
                       class = "btn btn-primary")
        )
      ))
      session$userData$editing_user <- is_edit
    }

    observeEvent(input$new_user, {
      current_actor <- actor()
      assert_permission(current_actor, "manage_users")
      open_user_modal(current_actor)
    })

    observeEvent(input$edit_user_id, {
      current_actor <- actor()
      assert_permission(current_actor, "manage_users")
      users <- admin_list_users(current_actor)
      user <- users[users$id == as.integer(input$edit_user_id), , drop = FALSE]
      if (nrow(user) == 1L) open_user_modal(current_actor, user)
    })

    observeEvent(input$save_user, {
      current_actor <- actor()
      assert_permission(current_actor, "manage_users")
      tryCatch({
        if (isTRUE(session$userData$editing_user)) {
          organization_id <- if (identical(current_actor$role, "super_admin")) {
            as.integer(input$user_organization_id)
          } else {
            current_actor$organization_id
          }
          admin_update_user(input$user_id, organization_id, input$user_role,
                            input$user_status, current_actor,
                            if (nzchar(input$user_new_password %||% "")) {
                              if (!identical(input$user_new_password, input$user_new_password_confirmation)) {
                                stop("Les deux nouveaux mots de passe ne correspondent pas.", call. = FALSE)
                              }
                              input$user_new_password
                            } else NULL)
          showNotification("Utilisateur mis à jour.", type = "message")
        } else {
          admin_create_user(list(
            first_name = input$user_first_name,
            last_name = input$user_last_name,
            email = input$user_email,
            role = input$user_role,
            organization_id = input$user_organization_id,
            organization_name = input$user_organization_name,
            password = input$user_password,
            password_confirmation = input$user_password_confirmation
          ), current_actor)
          showNotification("Compte créé. Le mot de passe n'est pas conservé en clair.", type = "message")
        }
        removeModal()
        session$userData$editing_user <- FALSE
        refresh(refresh() + 1)
      }, error = function(e) {
        message("user management error: ", e$message)
        showNotification(conditionMessage(e), type = "error")
      })
    })
  })
}
