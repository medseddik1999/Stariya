auth_roles <- c("super_admin", "owner", "admin", "member", "client")

auth_setup_required <- function() {

db_init()

  count <- db_query(

    "SELECT COUNT(*) AS count FROM users

     WHERE password_hash IS NOT NULL AND password_hash <> ''"

  )$count[[1]]

  count == 0L

}

auth_hash_password <- function(password) {

  if (length(password) != 1L || is.na(password) || nchar(password) < 12L) {

    stop("Le mot de passe doit contenir au moins 12 caractères.", call. = FALSE)

  }

  scrypt::hashPassword(password)

}

auth_create_first_admin <- function(first_name, last_name, email, password, confirmation, organization_name = NULL) {

  first_name <- trimws(first_name %||% "")

  last_name <- trimws(last_name %||% "")

  email <- tolower(trimws(email %||% ""))

  if (!nzchar(first_name) || !nzchar(last_name) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {

    stop("Renseignez un nom, un prénom et une adresse e-mail valide.", call. = FALSE)

  }

  if (!identical(password, confirmation)) {

    stop("Les deux mots de passe ne correspondent pas.", call. = FALSE)

  }

  password_hash <- auth_hash_password(password)

  con <- pool::poolCheckout(get_pool())

  on.exit(pool::poolReturn(con), add = TRUE)

  DBI::dbBegin(con)

  committed <- FALSE

  on.exit(if (!committed) try(DBI::dbRollback(con), silent = TRUE), add = TRUE)

  active_accounts <- DBI::dbGetQuery(

    con,

    "SELECT COUNT(*) AS count FROM users

     WHERE password_hash IS NOT NULL AND password_hash <> '' AND status = 'active'"

  )$count[[1]]

  if (active_accounts > 0L) {

    stop("Le compte initial a déjà été créé.", call. = FALSE)

  }

  organization_name <- trimws(organization_name %||% "")

  organization <- if (nzchar(organization_name)) {

    DBI::dbGetQuery(con, "SELECT id FROM organizations WHERE lower(name) = lower(?) LIMIT 1", params = list(organization_name))

  } else {

    DBI::dbGetQuery(con, "SELECT id FROM organizations ORDER BY id LIMIT 1")

  }

  if (nrow(organization) == 0L) {

    organization_name <- organization_name %||% "Startup OS"

    DBI::dbExecute(con, "INSERT INTO organizations (name) VALUES (?)", params = list(organization_name))

    organization_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]]

  } else {

    organization_id <- organization$id[[1]]

  }

  existing <- DBI::dbGetQuery(con, "SELECT id FROM users WHERE lower(email) = ? LIMIT 1", params = list(email))

  if (nrow(existing) > 0L) {

    user_id <- existing$id[[1]]

    DBI::dbExecute(

      con,

      "UPDATE users SET organization_id = ?, first_name = ?, last_name = ?,

       role = 'super_admin', role_id = NULL, password_hash = ?, status = 'active',

       failed_attempts = 0, locked_until = NULL, updated_at = CURRENT_TIMESTAMP

       WHERE id = ?",

      params = list(organization_id, first_name, last_name, password_hash, user_id)

    )

  } else {

    DBI::dbExecute(

      con,

      "INSERT INTO users (organization_id, first_name, last_name, email, role,

       password_hash, status, failed_attempts)

       VALUES (?, ?, ?, ?, 'super_admin', ?, 'active', 0)",

      params = list(organization_id, first_name, last_name, email, password_hash)

    )

  }

  DBI::dbCommit(con)

  committed <- TRUE

  invisible(TRUE)

}

auth_credentials <- function(user, password) {

  login <- tolower(trimws(as.character(user %||% "")))

  row <- db_query(

    "SELECT id, organization_id, first_name, last_name, email, role,

            password_hash, failed_attempts, locked_until, status

     FROM users WHERE lower(email) = ? LIMIT 1",

    list(login)

  )

  failure <- list(result = FALSE, expired = FALSE, authorized = TRUE, user_info = NULL)

  if (nrow(row) != 1L || row$status[[1]] != "active") return(failure)

  locked_until <- row$locked_until[[1]]

  if (!is.na(locked_until) && nzchar(locked_until) && as.POSIXct(locked_until, tz = "UTC") > Sys.time()) {

    return(failure)

  }

  password_ok <- tryCatch(

    isTRUE(scrypt::verifyPassword(row$password_hash[[1]], as.character(password))),

    error = function(e) FALSE

  )

  if (!password_ok) {

    db_execute(

      "UPDATE users SET failed_attempts = failed_attempts + 1,

       locked_until = CASE WHEN failed_attempts + 1 >= 5

         THEN datetime('now', '+15 minutes') ELSE locked_until END

       WHERE id = ? AND organization_id = ?",

      list(row$id[[1]], row$organization_id[[1]])

    )

    return(failure)

  }

  if (!row$role[[1]] %in% auth_roles) return(failure)

  db_execute(

    "UPDATE users SET failed_attempts = 0, locked_until = NULL,

     last_login_at = CURRENT_TIMESTAMP WHERE id = ? AND organization_id = ?",

    list(row$id[[1]], row$organization_id[[1]])

  )

  user_info <- data.frame(

    user = row$email[[1]],

    user_id = as.integer(row$id[[1]]),

    organization_id = as.integer(row$organization_id[[1]]),

    role = row$role[[1]],

    first_name = row$first_name[[1]],

    last_name = row$last_name[[1]],

    stringsAsFactors = FALSE

  )

  list(result = TRUE, expired = FALSE, authorized = TRUE, user_info = user_info)

}

auth_actor <- function(authenticated_user) {

  required <- c("user_id", "organization_id", "role")

  if (!all(required %in% names(authenticated_user)) ||

      !authenticated_user$role %in% auth_roles ||

      length(authenticated_user$user_id) != 1L ||

      length(authenticated_user$organization_id) != 1L) {

    stop("Session d'authentification invalide.", call. = FALSE)

  }

  list(

    user_id = as.integer(authenticated_user$user_id),

    organization_id = as.integer(authenticated_user$organization_id),

    role = as.character(authenticated_user$role),

    email = as.character(authenticated_user$user %||% "")

  )

}

role_can <- function(actor, permission) {

  permissions <- list(

    super_admin = c("read_dashboard", "read_projects", "write_projects", "delete_projects", "read_tasks", "write_tasks", "delete_tasks", "read_customers", "write_customers", "delete_customers", "read_activities", "manage_users", "manage_organizations"),

    owner = c("read_dashboard", "read_projects", "write_projects", "delete_projects", "read_tasks", "write_tasks", "delete_tasks", "read_customers", "write_customers", "delete_customers", "read_activities", "read_users", "manage_users"),

    admin = c("read_dashboard", "read_projects", "write_projects", "delete_projects", "read_tasks", "write_tasks", "delete_tasks", "read_customers", "write_customers", "delete_customers", "read_activities", "read_users", "manage_users"),

    member = c("read_dashboard", "read_projects", "read_tasks", "write_tasks", "read_activities", "read_users"),

    client = c("read_tasks")

  )

  !is.null(actor$role) && permission %in% permissions[[actor$role]]

}

assert_permission <- function(actor, permission) {

  if (!role_can(actor, permission)) {

    stop("Accès refusé.", call. = FALSE)

  }

  invisible(TRUE)

}

auth_setup_ui <- function() {

  shiny::fluidPage(

    shiny::tags$head(

      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),

      shiny::tags$link(rel = "stylesheet", href = "css/custom.css?v=11")

    ),

    shiny::div(class = "auth-setup-page",

      shiny::div(class = "auth-setup-panel",

        shiny::div(class = "auth-brand",

          shiny::span(class = "logo-mark", "S"),

          shiny::span(class = "logo-text", "Startup OS")

        ),

        shiny::h1("Configurer Startup OS"),

        shiny::p("Créez le compte super-administrateur pour sécuriser votre espace."),

        shiny::textInput("setup_first_name", "Prénom"),

        shiny::textInput("setup_last_name", "Nom"),

        shiny::textInput("setup_email", "Adresse e-mail"),

        shiny::textInput("setup_organization", "Organisation", value = "Acme Startup"),

        shiny::passwordInput("setup_password", "Mot de passe (12 caractères minimum)"),

        shiny::passwordInput("setup_confirmation", "Confirmer le mot de passe"),

        shiny::uiOutput("setup_error"),

        shiny::actionButton("setup_submit", "Créer le compte sécurisé", class = "btn btn-primary")

      )

    )

  )

}

auth_setup_server <- function(input, output, session) {

  setup_error <- shiny::reactiveVal(NULL)

  output$setup_error <- shiny::renderUI({

    if (is.null(setup_error())) return(NULL)

    shiny::div(class = "auth-error", setup_error())

  })

  shiny::observeEvent(input$setup_submit, {

    tryCatch({

      auth_create_first_admin(

        input$setup_first_name,

        input$setup_last_name,

        input$setup_email,

        input$setup_password,

        input$setup_confirmation,

        input$setup_organization

      )

      session$reload()

    }, error = function(e) {

      setup_error(conditionMessage(e))

    })

  })

}

admin_list_users <- function(actor) {

  assert_permission(actor, "manage_users")

  if (identical(actor$role, "super_admin")) {

    db_query(

      "SELECT u.id, u.organization_id, o.name AS organization_name,

              u.first_name, u.last_name, u.email, u.role, u.status, u.last_login_at

       FROM users u JOIN organizations o ON o.id = u.organization_id

       ORDER BY o.name, u.email"

    )

  } else {

    db_query(

      "SELECT u.id, u.organization_id, o.name AS organization_name,

              u.first_name, u.last_name, u.email, u.role, u.status, u.last_login_at

       FROM users u JOIN organizations o ON o.id = u.organization_id

       WHERE u.organization_id = ? ORDER BY u.email",

      list(actor$organization_id)

    )

  }

}

admin_create_user <- function(data, actor) {

  assert_permission(actor, "manage_users")

  email <- tolower(trimws(data$email %||% ""))

  role <- as.character(data$role %||% "member")

  first_name <- trimws(data$first_name %||% "")

  last_name <- trimws(data$last_name %||% "")

  if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email) || !nzchar(first_name) || !nzchar(last_name)) {

    stop("Renseignez un nom, un prénom et une adresse e-mail valide.", call. = FALSE)

  }

  if (!role %in% auth_roles || identical(role, "super_admin") && !identical(actor$role, "super_admin")) {

    stop("Ce rôle ne peut pas être attribué par votre compte.", call. = FALSE)

  }

  if (actor$role %in% c("owner", "admin") && role == "owner") {

    stop("Seul un super-administrateur peut attribuer le rôle owner.", call. = FALSE)

  }

  if (!identical(data$password, data$password_confirmation)) {

    stop("Les deux mots de passe ne correspondent pas.", call. = FALSE)

  }

  password_hash <- auth_hash_password(data$password)

  con <- pool::poolCheckout(get_pool())

  on.exit(pool::poolReturn(con), add = TRUE)

  DBI::dbBegin(con)

  committed <- FALSE

  on.exit(if (!committed) try(DBI::dbRollback(con), silent = TRUE), add = TRUE)

  if (nrow(DBI::dbGetQuery(con, "SELECT id FROM users WHERE lower(email) = ?", params = list(email))) > 0L) {

    stop("Cette adresse e-mail possède déjà un compte.", call. = FALSE)

  }

  organization_name <- if (identical(actor$role, "super_admin")) trimws(data$organization_name %||% "") else ""

  if (nzchar(organization_name)) {

    DBI::dbExecute(con, "INSERT INTO organizations (name) VALUES (?)", params = list(organization_name))

    organization_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]]

  } else if (identical(actor$role, "super_admin")) {

    organization_id <- as.integer(data$organization_id)

    organization <- DBI::dbGetQuery(con, "SELECT id FROM organizations WHERE id = ?", params = list(organization_id))

    if (nrow(organization) != 1L) stop("Organisation invalide.", call. = FALSE)

  } else {

    organization_id <- actor$organization_id

  }

  DBI::dbExecute(

    con,

    "INSERT INTO users (organization_id, first_name, last_name, email, role,

     password_hash, status, failed_attempts)

     VALUES (?, ?, ?, ?, ?, ?, 'active', 0)",

    params = list(organization_id, first_name, last_name, email, role, password_hash)

  )

  user_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]]

  DBI::dbCommit(con)

  committed <- TRUE

  user_id

}

admin_list_organizations <- function(actor) {

  assert_permission(actor, "manage_users")

  if (identical(actor$role, "super_admin")) {

    db_query("SELECT id, name FROM organizations ORDER BY name")

  } else {

    db_query("SELECT id, name FROM organizations WHERE id = ?", list(actor$organization_id))

  }

}

admin_update_user <- function(user_id, organization_id, role, status, actor, new_password = NULL) {

  assert_permission(actor, "manage_users")

  if (!role %in% auth_roles || !status %in% c("active", "disabled")) {

    stop("Rôle ou statut invalide.", call. = FALSE)

  }

  if (actor$role != "super_admin" && role %in% c("super_admin", "owner")) {

    stop("Ce rôle ne peut pas être attribué par votre compte.", call. = FALSE)

  }

  if (actor$role != "super_admin" && as.integer(organization_id) != actor$organization_id) {

    stop("Accès refusé.", call. = FALSE)

  }

  if (as.integer(user_id) == actor$user_id && status != "active") {

    stop("Vous ne pouvez pas désactiver votre propre compte.", call. = FALSE)

  }

  existing <- db_query(

    "SELECT id FROM users WHERE id = ? AND organization_id = ?",

    list(as.integer(user_id), as.integer(organization_id))

  )

  if (nrow(existing) != 1L) stop("Utilisateur introuvable.", call. = FALSE)

  if (!is.null(new_password) && nzchar(new_password)) {

    db_execute(

      "UPDATE users SET role = ?, status = ?, password_hash = ?, failed_attempts = 0,

       locked_until = NULL, updated_at = CURRENT_TIMESTAMP

       WHERE id = ? AND organization_id = ?",

      list(role, status, auth_hash_password(new_password), as.integer(user_id), as.integer(organization_id))

    )

  } else {

    db_execute(

      "UPDATE users SET role = ?, status = ?, updated_at = CURRENT_TIMESTAMP

       WHERE id = ? AND organization_id = ?",

      list(role, status, as.integer(user_id), as.integer(organization_id))

   )

 }

}
