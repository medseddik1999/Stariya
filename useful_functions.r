creer_super_admin_midou <- function() {
  db_init()

  password <- "789452789452TIARET"
  password_hash <- auth_hash_password(password)

  con <- pool::poolCheckout(get_pool())
  on.exit(pool::poolReturn(con), add = TRUE)

  DBI::dbBegin(con)
  committed <- FALSE
  on.exit(if (!committed) try(DBI::dbRollback(con), silent = TRUE), add = TRUE)

  organization <- DBI::dbGetQuery(
    con,
    "SELECT id FROM organizations ORDER BY id LIMIT 1"
  )

  if (nrow(organization) == 0L) {
    DBI::dbExecute(
      con,
      "INSERT INTO organizations (name) VALUES (?)",
      params = list("Startup OS")
    )
    organization_id <- DBI::dbGetQuery(
      con,
      "SELECT last_insert_rowid() AS id"
    )$id[[1]]
  } else {
    organization_id <- organization$id[[1]]
  }

  email <- "midou@startup-os.local"

  existing <- DBI::dbGetQuery(
    con,
    "SELECT id FROM users WHERE lower(email) = ? LIMIT 1",
    params = list(email)
  )

  if (nrow(existing) > 0L) {
    DBI::dbExecute(
      con,
      "UPDATE users
       SET organization_id = ?,
           first_name = 'Midou',
           last_name = 'Admin',
           role = 'super_admin',
           role_id = NULL,
           password_hash = ?,
           status = 'active',
           failed_attempts = 0,
           locked_until = NULL,
           updated_at = CURRENT_TIMESTAMP
       WHERE id = ?",
      params = list(organization_id, password_hash, existing$id[[1]])
    )
  } else {
    DBI::dbExecute(
      con,
      "INSERT INTO users (
         organization_id, first_name, last_name, email,
         role, password_hash, status, failed_attempts
       )
       VALUES (?, 'Midou', 'Admin', ?, 'super_admin', ?, 'active', 0)",
      params = list(organization_id, email, password_hash)
    )
  }

  DBI::dbCommit(con)
  committed <- TRUE

  message("Super-admin Midou créé avec succès.")
  invisible(TRUE)
}