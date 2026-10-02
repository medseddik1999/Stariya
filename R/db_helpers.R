# ---------- low level helpers ----------
db_query <- function(sql, params = list()) {
  pool <- get_pool()
  con  <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  if (length(params)) {
    DBI::dbGetQuery(con, sql, params = params)
  } else {
    DBI::dbGetQuery(con, sql)
  }
}

db_execute <- function(sql, params = list()) {
  pool <- get_pool()
  con  <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  if (length(params)) {
    DBI::dbExecute(con, sql, params = params)
  } else {
    DBI::dbExecute(con, sql)
  }
}

db_insert <- function(sql, params = list()) {
  pool <- get_pool()
  con  <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  if (length(params)) {
    DBI::dbExecute(con, sql, params = params)
  } else {
    DBI::dbExecute(con, sql)
  }
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
}

# ---------- activity log ----------
require_tenant_record <- function(table, id, actor) {
  assert_permission(actor, "read_dashboard")
  allowed_tables <- c("projects", "tasks", "customers")
  if (!table %in% allowed_tables) stop("Table non autorisee.", call. = FALSE)
  exists <- db_query(
    sprintf("SELECT 1 FROM %s WHERE id = ? AND organization_id = ? LIMIT 1", table),
    list(as.integer(id), actor$organization_id)
  )
  if (nrow(exists) != 1L) stop("Ressource introuvable.", call. = FALSE)
  invisible(TRUE)
}

log_activity <- function(type, description, actor,
                         entity_type = NULL, entity_id = NULL) {
  db_execute(
    "INSERT INTO activities
       (organization_id, user_id, type, description, entity_type, entity_id)
     VALUES (?, ?, ?, ?, ?, ?)",
    list(actor$organization_id, actor$user_id, type, description, entity_type,
         if (is.null(entity_id)) NA else as.integer(entity_id))
  )
}

get_activities <- function(actor, limit = 20) {
  assert_permission(actor, "read_activities")
  db_query(
    sprintf(
      "SELECT a.id, a.type, a.description, a.created_at,
              COALESCE(u.first_name || ' ' || u.last_name, 'System') AS user_name
       FROM activities a
       LEFT JOIN users u ON u.id = a.user_id
       WHERE a.organization_id = ?
       ORDER BY a.created_at DESC, a.id DESC
       LIMIT %d", as.integer(limit)),
    list(actor$organization_id)
  )
}

# ---------- users ----------
get_users <- function(actor) {
  assert_permission(actor, "read_users")
  db_query(
    "SELECT id, first_name, last_name, email,
            first_name || ' ' || last_name AS full_name
     FROM users WHERE organization_id = ? AND status = 'active' ORDER BY first_name",
    list(actor$organization_id)
  )
}

# ---------- projects ----------
get_projects <- function(actor) {
  assert_permission(actor, "read_projects")
  db_query(
    "SELECT p.id, p.name, p.description, p.status, p.priority,
            p.progress, p.start_date, p.end_date,
            COALESCE(u.first_name || ' ' || u.last_name, '—') AS owner_name,
            p.owner_id
     FROM projects p
    LEFT JOIN users u ON u.id = p.owner_id AND u.organization_id = p.organization_id
     WHERE p.organization_id = ?
     ORDER BY p.created_at DESC",
    list(actor$organization_id)
  )
}

get_project <- function(id, actor) {
  assert_permission(actor, "read_projects")
  db_query("SELECT * FROM projects WHERE id = ? AND organization_id = ?", list(as.integer(id), actor$organization_id))
}

create_project <- function(data, actor) {
  assert_permission(actor, "write_projects")
  if (!is.null(data$owner_id) && nzchar(as.character(data$owner_id))) {
    owner <- db_query(
      "SELECT id FROM users WHERE id = ? AND organization_id = ? AND status = 'active'",
      list(as.integer(data$owner_id), actor$organization_id)
    )
    if (nrow(owner) != 1L) stop("Responsable invalide pour cette organisation.", call. = FALSE)
  }
  db_insert(
    "INSERT INTO projects
       (organization_id, name, description, status, priority,
        owner_id, start_date, end_date, progress)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    list(actor$organization_id, data$name, data$description %||% NA,
         data$status %||% "planning", data$priority %||% "medium",
         if (is.null(data$owner_id) || is.na(data$owner_id) || data$owner_id == "") NA else as.integer(data$owner_id),
         data$start_date %||% NA, data$end_date %||% NA,
         as.integer(data$progress %||% 0))
  )
  log_activity("project_created", paste0("Project '", data$name, "' created"), actor, "project")
}

update_project <- function(id, data, actor) {
  assert_permission(actor, "write_projects")
  require_tenant_record("projects", id, actor)
  if (!is.null(data$owner_id) && nzchar(as.character(data$owner_id))) {
    owner <- db_query(
      "SELECT id FROM users WHERE id = ? AND organization_id = ? AND status = 'active'",
      list(as.integer(data$owner_id), actor$organization_id)
    )
    if (nrow(owner) != 1L) stop("Responsable invalide pour cette organisation.", call. = FALSE)
  }
  db_execute(
    "UPDATE projects
     SET name = ?, description = ?, status = ?, priority = ?,
         owner_id = ?, start_date = ?, end_date = ?, progress = ?,
         updated_at = CURRENT_TIMESTAMP
    WHERE id = ? AND organization_id = ?",
    list(data$name, data$description %||% NA, data$status, data$priority,
         if (is.null(data$owner_id) || is.na(data$owner_id) || data$owner_id == "") NA else as.integer(data$owner_id),
         data$start_date %||% NA, data$end_date %||% NA,
         as.integer(data$progress %||% 0), id, actor$organization_id)
  )
  log_activity("project_updated", paste0("Project '", data$name, "' updated"), actor, "project", id)
}

delete_project <- function(id, actor) {
  assert_permission(actor, "delete_projects")
  require_tenant_record("projects", id, actor)
  db_execute("DELETE FROM projects WHERE id = ? AND organization_id = ?", list(id, actor$organization_id))
  log_activity("project_deleted", paste0("Project #", id, " deleted"), actor, "project", id)
}

# ---------- tasks ----------
get_tasks <- function(actor, project_id = NULL) {
  assert_permission(actor, "read_tasks")
  sql <- "SELECT t.id, t.title, t.description, t.status, t.priority,
                 t.due_date, t.project_id, t.assigned_to,
       t.parent_task_id,
                 p.name AS project_name,
       COALESCE(u.first_name || ' ' || u.last_name, '—') AS assignee_name,
       (SELECT COUNT(*) FROM tasks child WHERE child.parent_task_id = t.id) AS subtask_count
          FROM tasks t
          LEFT JOIN projects p ON p.id = t.project_id AND p.organization_id = t.organization_id
          LEFT JOIN users u    ON u.id = t.assigned_to AND u.organization_id = t.organization_id
     WHERE t.organization_id = ? AND t.parent_task_id IS NULL"
  params <- list(actor$organization_id)
  if (identical(actor$role, "client")) {
    sql <- paste0(sql, " AND t.assigned_to = ?")
    params <- c(params, list(actor$user_id))
  }
  if (!is.null(project_id) && !is.na(project_id) && project_id > 0) {
    sql <- paste0(sql, " AND t.project_id = ?")
    params <- c(params, list(as.integer(project_id)))
  }
  sql <- paste0(sql, " ORDER BY t.created_at DESC")
  db_query(sql, params)
}

get_task <- function(id, actor) {
  assert_permission(actor, "read_tasks")
  sql <- "SELECT t.id, t.title, t.description, t.status, t.priority,
                 t.due_date, t.project_id, t.assigned_to, t.parent_task_id,
                 p.name AS project_name,
                 COALESCE(u.first_name || ' ' || u.last_name, '—') AS assignee_name
          FROM tasks t
          LEFT JOIN projects p ON p.id = t.project_id AND p.organization_id = t.organization_id
          LEFT JOIN users u ON u.id = t.assigned_to AND u.organization_id = t.organization_id
          WHERE t.id = ? AND t.organization_id = ?"
  params <- list(as.integer(id), actor$organization_id)
  if (identical(actor$role, "client")) {
    sql <- paste0(sql, " AND t.assigned_to = ?")
    params <- c(params, list(actor$user_id))
  }
  db_query(
    sql,
    params
  )
}

get_subtasks <- function(parent_id, actor) {
  assert_permission(actor, "read_tasks")
  parent <- get_task(parent_id, actor)
  if (nrow(parent) != 1L) return(data.frame())
  db_query(
    "SELECT t.id, t.title, t.status, t.priority, t.due_date,
            t.project_id, t.assigned_to, t.parent_task_id,
            p.name AS project_name,
            COALESCE(u.first_name || ' ' || u.last_name, '—') AS assignee_name
     FROM tasks t
    LEFT JOIN projects p ON p.id = t.project_id AND p.organization_id = t.organization_id
    LEFT JOIN users u ON u.id = t.assigned_to AND u.organization_id = t.organization_id
     WHERE t.parent_task_id = ? AND t.organization_id = ?
       AND (? <> 'client' OR t.assigned_to = ?)
     ORDER BY t.created_at, t.id",
    list(as.integer(parent_id), actor$organization_id, actor$role, actor$user_id)
  )
}

create_task <- function(data, actor) {
  assert_permission(actor, "write_tasks")
  if (identical(actor$role, "member")) data$assigned_to <- actor$user_id
  if (identical(actor$role, "client")) stop("Accès refusé.", call. = FALSE)
  if (!is.null(data$parent_task_id) && !is.na(data$parent_task_id) && data$parent_task_id != "") {
    parent <- get_task(data$parent_task_id, actor)
    if (nrow(parent) != 1L || !is.na(parent$parent_task_id[[1]])) {
      stop("Tâche parente invalide.", call. = FALSE)
    }
  }
  if (!is.null(data$project_id) && !is.na(data$project_id) && data$project_id != "") {
    project <- db_query("SELECT id FROM projects WHERE id = ? AND organization_id = ?",
                        list(as.integer(data$project_id), actor$organization_id))
    if (nrow(project) != 1L) stop("Projet invalide pour cette organisation.", call. = FALSE)
  }
  if (!is.null(data$assigned_to) && !is.na(data$assigned_to) && data$assigned_to != "") {
    assignee <- db_query(
      "SELECT id FROM users WHERE id = ? AND organization_id = ? AND status = 'active'",
      list(as.integer(data$assigned_to), actor$organization_id)
    )
    if (nrow(assignee) != 1L) stop("Responsable invalide pour cette organisation.", call. = FALSE)
  }
  task_id <- db_insert(
    "INSERT INTO tasks
       (organization_id, project_id, title, description, status,
        priority, assigned_to, due_date, parent_task_id)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    list(actor$organization_id,
         if (is.null(data$project_id) || is.na(data$project_id) || data$project_id == "") NA else as.integer(data$project_id),
         data$title, data$description %||% NA,
         data$status %||% "todo", data$priority %||% "medium",
         if (is.null(data$assigned_to) || is.na(data$assigned_to) || data$assigned_to == "") NA else as.integer(data$assigned_to),
         data$due_date %||% NA,
         if (is.null(data$parent_task_id) || is.na(data$parent_task_id) || data$parent_task_id == "") NA else as.integer(data$parent_task_id))
  )
  log_activity("task_created", paste0("Task '", data$title, "' created"), actor, "task", task_id)
  task_id
}

update_task <- function(id, data, actor) {
  assert_permission(actor, "write_tasks")
  task <- get_task(id, actor)
  if (nrow(task) != 1L) stop("Tâche introuvable.", call. = FALSE)
  if (identical(actor$role, "member") && !identical(as.integer(task$assigned_to[[1]]), actor$user_id)) {
    stop("Vous ne pouvez modifier que vos propres tâches.", call. = FALSE)
  }
  if (identical(actor$role, "member")) data$assigned_to <- actor$user_id
  if (!is.null(data$project_id) && !is.na(data$project_id) && data$project_id != "") {
    project <- db_query("SELECT id FROM projects WHERE id = ? AND organization_id = ?",
                        list(as.integer(data$project_id), actor$organization_id))
    if (nrow(project) != 1L) stop("Projet invalide pour cette organisation.", call. = FALSE)
  }
  if (!is.null(data$assigned_to) && !is.na(data$assigned_to) && data$assigned_to != "") {
    assignee <- db_query(
      "SELECT id FROM users WHERE id = ? AND organization_id = ? AND status = 'active'",
      list(as.integer(data$assigned_to), actor$organization_id)
    )
    if (nrow(assignee) != 1L) stop("Responsable invalide pour cette organisation.", call. = FALSE)
  }
  db_execute(
    "UPDATE tasks
     SET project_id = ?, title = ?, description = ?, status = ?,
         priority = ?, assigned_to = ?, due_date = ?,
         updated_at = CURRENT_TIMESTAMP
    WHERE id = ? AND organization_id = ?",
    list(
      if (is.null(data$project_id) || is.na(data$project_id) || data$project_id == "") NA else as.integer(data$project_id),
      data$title, data$description %||% NA, data$status, data$priority,
      if (is.null(data$assigned_to) || is.na(data$assigned_to) || data$assigned_to == "") NA else as.integer(data$assigned_to),
      data$due_date %||% NA, id, actor$organization_id)
  )
  log_activity("task_updated", paste0("Task '", data$title, "' updated"), actor, "task", id)
}

update_task_status <- function(id, status, actor) {
  assert_permission(actor, "write_tasks")
  if (!status %in% c("todo", "in_progress", "review", "done")) stop("Statut invalide.", call. = FALSE)
  task <- get_task(id, actor)
  if (nrow(task) != 1L) stop("Tâche introuvable.", call. = FALSE)
  if (identical(actor$role, "member") && !identical(as.integer(task$assigned_to[[1]]), actor$user_id)) {
    stop("Vous ne pouvez modifier que vos propres tâches.", call. = FALSE)
  }
  db_execute("UPDATE tasks SET status = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ? AND organization_id = ?",
             list(status, id, actor$organization_id))
  log_activity("task_moved", paste0("Task #", id, " -> ", status), actor, "task", id)
}

delete_task <- function(id, actor) {
  assert_permission(actor, "delete_tasks")
  require_tenant_record("tasks", id, actor)
  db_execute("DELETE FROM tasks WHERE organization_id = ? AND (id = ? OR parent_task_id = ?)",
             list(actor$organization_id, id, id))
  log_activity("task_deleted", paste0("Task #", id, " deleted"), actor, "task", id)
}

# ---------- customers ----------
get_customers <- function(actor) {
  assert_permission(actor, "read_customers")
  db_query(
    "SELECT id, company_name, contact_name, email, phone, status, segment,
            created_at
     FROM customers
     WHERE organization_id = ?
     ORDER BY created_at DESC",
    list(actor$organization_id)
  )
}

get_customer <- function(id, actor) {
  assert_permission(actor, "read_customers")
  db_query("SELECT * FROM customers WHERE id = ? AND organization_id = ?", list(as.integer(id), actor$organization_id))
}

create_customer <- function(data, actor) {
  assert_permission(actor, "write_customers")
  db_insert(
    "INSERT INTO customers
       (organization_id, company_name, contact_name, email, phone, status, segment)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    list(actor$organization_id, data$company_name, data$contact_name %||% NA,
         data$email %||% NA, data$phone %||% NA,
         data$status %||% "active", data$segment %||% NA)
  )
  log_activity("customer_added", paste0("Customer '", data$company_name, "' added"), actor, "customer")
}

update_customer <- function(id, data, actor) {
  assert_permission(actor, "write_customers")
  require_tenant_record("customers", id, actor)
  db_execute(
    "UPDATE customers
     SET company_name = ?, contact_name = ?, email = ?, phone = ?,
         status = ?, segment = ?, updated_at = CURRENT_TIMESTAMP
    WHERE id = ? AND organization_id = ?",
    list(data$company_name, data$contact_name %||% NA,
         data$email %||% NA, data$phone %||% NA,
         data$status, data$segment %||% NA, id, actor$organization_id)
  )
  log_activity("customer_updated", paste0("Customer '", data$company_name, "' updated"), actor, "customer", id)
}

delete_customer <- function(id, actor) {
  assert_permission(actor, "delete_customers")
  require_tenant_record("customers", id, actor)
  db_execute("DELETE FROM customers WHERE id = ? AND organization_id = ?", list(id, actor$organization_id))
  log_activity("customer_deleted", paste0("Customer #", id, " deleted"), actor, "customer", id)
}

# ---------- KPIs ----------
get_kpis <- function(actor) {
  assert_permission(actor, "read_dashboard")
  db_query("SELECT * FROM kpis WHERE organization_id = ? ORDER BY id", list(actor$organization_id))
}

# ---------- dashboard stats ----------
dashboard_stats <- function(actor) {
  assert_permission(actor, "read_dashboard")
  list(
    revenue     = db_query("SELECT COALESCE(SUM(value),0) AS v FROM opportunities
                            WHERE organization_id = ? AND stage = 'won'", list(actor$organization_id))$v,
    customers   = db_query("SELECT COUNT(*) AS n FROM customers
                            WHERE organization_id = ? AND status = 'active'", list(actor$organization_id))$n,
    projects    = db_query("SELECT COUNT(*) AS n FROM projects
                            WHERE organization_id = ? AND status = 'in_progress'", list(actor$organization_id))$n,
    open_tasks  = db_query("SELECT COUNT(*) AS n FROM tasks
                            WHERE organization_id = ? AND status != 'done'", list(actor$organization_id))$n
  )
}

opportunity_series <- function(actor) {
  assert_permission(actor, "read_dashboard")
  db_query(
    "SELECT name, value, stage
     FROM opportunities
     WHERE organization_id = ?
    ORDER BY value DESC, name", list(actor$organization_id)
  )
}