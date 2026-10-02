.db_env <- new.env()
.db_env$pool <- NULL

db_init <- function() {
  if (!is.null(.db_env$pool)) {
    return(invisible(.db_env$pool))
  }
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)

  .db_env$pool <- pool::dbPool(
    drv     = RSQLite::SQLite(),
    dbname  = DB_PATH,
    minSize = 1,
    maxSize = 5
  )

  create_schema(.db_env$pool)
  migrate_task_hierarchy(.db_env$pool)
  migrate_auth_schema(.db_env$pool)
  seed_if_empty(.db_env$pool)

  invisible(.db_env$pool)
}

get_pool <- function() {
  if (is.null(.db_env$pool)) db_init()
  .db_env$pool
}

db_close <- function() {
  if (!is.null(.db_env$pool)) {
    pool::poolClose(.db_env$pool)
    .db_env$pool <- NULL
  }
}

run_sql_file <- function(con, path) {
  sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
  stmts <- strsplit(sql, ";\\s*\n", perl = TRUE)[[1]]
  stmts <- trimws(stmts)
  stmts <- stmts[nzchar(stmts)]
  for (s in stmts) DBI::dbExecute(con, s)
}

create_schema <- function(pool) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  run_sql_file(con, SCHEMA_PATH)
}

migrate_task_hierarchy <- function(pool) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  if (!"parent_task_id" %in% DBI::dbListFields(con, "tasks")) {
    DBI::dbExecute(con, "ALTER TABLE tasks ADD COLUMN parent_task_id INTEGER")
  }
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_tasks_parent ON tasks(parent_task_id)")
}

seed_if_empty <- function(pool) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)

  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM organizations")$n
  if (n > 0) return(invisible())

  message("Startup OS: seeding demo data...")

  # --- organization
  DBI::dbExecute(con, "INSERT INTO organizations (id, name) VALUES (1, 'Acme Startup')")

  # --- roles
  DBI::dbExecute(con, "INSERT INTO roles (id, name, description) VALUES
    (1, 'Admin',   'Full access'),
    (2, 'Manager', 'Manage projects and team'),
    (3, 'Member',  'Standard access')")

  # --- users
  DBI::dbExecute(con, "INSERT INTO users (id, organization_id, first_name, last_name, email, role_id, status) VALUES
    (1, 1, 'John', 'Doe',    'john@acme.io',  1, 'active'),
    (2, 1, 'Jane', 'Smith',  'jane@acme.io',  2, 'active'),
    (3, 1, 'Bob',  'Wilson', 'bob@acme.io',   3, 'active')")

  # --- projects
  DBI::dbExecute(con, "INSERT INTO projects
    (organization_id, name, description, status, priority, owner_id, start_date, end_date, progress) VALUES
    (1, 'Website Redesign',  'Refonte complète du site marketing', 'in_progress', 'high',   1, '2025-01-15', '2025-04-30', 65),
    (1, 'Mobile App v1',     'Application iOS/Android MVP',        'in_progress', 'medium', 2, '2025-02-01', '2025-06-15', 40),
    (1, 'API Integration',   'Connexion Stripe + HubSpot',         'planning',    'high',   1, '2025-03-01', '2025-05-30', 10),
    (1, 'Marketing Site',    'Landing pages produit',              'done',        'medium', 3, '2024-11-01', '2025-01-10', 100),
    (1, 'Data Pipeline',     'ETL analytics interne',              'in_progress', 'high',   2, '2025-01-05', '2025-04-15', 75)")

  # --- tasks
  DBI::dbExecute(con, "INSERT INTO tasks
    (organization_id, project_id, title, status, priority, assigned_to, due_date) VALUES
    (1, 1, 'Design home hero section',      'done',        'high',   1, '2025-02-10'),
    (1, 1, 'Implement responsive navbar',   'in_progress', 'medium', 2, '2025-03-05'),
    (1, 1, 'SEO audit + fixes',             'review',      'low',    3, '2025-03-20'),
    (1, 2, 'Setup React Native project',    'done',        'high',   2, '2025-02-08'),
    (1, 2, 'Auth screens',                  'in_progress', 'high',   1, '2025-03-12'),
    (1, 2, 'Push notifications',            'todo',        'medium', 2, '2025-04-01'),
    (1, 2, 'App store submission',          'todo',        'low',    3, '2025-05-20'),
    (1, 3, 'Stripe webhook handler',        'todo',        'high',   1, '2025-03-15'),
    (1, 3, 'HubSpot sync mapping',          'todo',        'medium', 2, '2025-03-28'),
    (1, 5, 'Define events taxonomy',        'done',        'high',   2, '2025-01-20'),
    (1, 5, 'Airflow DAG setup',             'in_progress', 'high',   2, '2025-03-10'),
    (1, 5, 'Dashboard Metabase',            'review',      'medium', 3, '2025-03-25')")

  # --- customers
  DBI::dbExecute(con, "INSERT INTO customers
    (organization_id, company_name, contact_name, email, phone, status, segment) VALUES
    (1, 'Northwind',       'Alice Martin',  'alice@northwind.io', '0102030405', 'active',   'Enterprise'),
    (1, 'Globex Corp',     'Marc Dupont',   'marc@globex.com',    '0104050607', 'active',   'Mid-market'),
    (1, 'Initech',         'Sarah Lee',     'sarah@initech.io',   '0108091011', 'active',   'SMB'),
    (1, 'Umbrella Labs',   'Tom Becker',    'tom@umbrella.co',    '0102030406', 'prospect', 'Enterprise'),
    (1, 'Soylent',         'Nina Roy',      'nina@soylent.io',    '0105060708', 'inactive', 'SMB')")

  # --- leads
  DBI::dbExecute(con, "INSERT INTO leads
    (organization_id, company_name, contact_name, email, source, status, owner_id) VALUES
    (1, 'Vandelay Industries', 'George C.',  'george@vandelay.com', 'website',   'qualified', 1),
    (1, 'Stark Industries',    'Pepper P.',  'pepper@stark.io',     'referral',  'new',       2),
    (1, 'Wayne Enterprises',   'Lucius F.',  'lucius@wayne.com',    'event',     'new',       1),
    (1, 'Pied Piper',          'Richard H.', 'richard@pp.io',       'cold',      'contacted', 3)")

  # --- opportunities
  DBI::dbExecute(con, "INSERT INTO opportunities
    (organization_id, name, value, stage, probability, expected_close_date, owner_id) VALUES
    (1, 'Northwind - Expansion',      25000, 'proposal',   60, '2025-04-15', 1),
    (1, 'Globex - Annual contract',   18000, 'negotiation', 75, '2025-04-01', 2),
    (1, 'Umbrella Labs - Pilot',       9000, 'qualified',  40, '2025-05-10', 1),
    (1, 'Initech - Renewal',           6000, 'won',       100, '2025-03-05', 3)")

  # --- campaigns
  DBI::dbExecute(con, "INSERT INTO campaigns
    (organization_id, name, description, status, budget, start_date, end_date) VALUES
    (1, 'Spring Launch',     'Lancement produit Q2',  'active',   8000, '2025-03-01', '2025-05-31'),
    (1, 'Content SEO Q1',    'Blog + SEO on-page',    'completed', 3500, '2025-01-01', '2025-03-31'),
    (1, 'Webinar Series',    '4 webinaires clients',  'planned',   4500, '2025-04-15', '2025-06-30')")

  # --- feedback
  DBI::dbExecute(con, "INSERT INTO feedback
    (organization_id, customer_id, type, content, priority, status) VALUES
    (1, 1, 'feature',  'Ajouter export CSV dans les rapports', 'medium', 'open'),
    (1, 2, 'bug',      'Lenteur sur la page projets',          'high',   'in_progress'),
    (1, 3, 'praise',   'Interface très claire, bravo !',       'low',    'closed')")

  # --- client_requests
  DBI::dbExecute(con, "INSERT INTO client_requests
    (organization_id, customer_id, title, description, status, priority, assigned_to) VALUES
    (1, 1, 'Accès SSO demandé',       'Support Okta SAML',       'open',        'medium', 1),
    (1, 2, 'Nouvelle intégration',    'Connexion à leur CRM',    'in_progress', 'high',   2),
    (1, 3, 'Formation équipe',        'Session onboarding',      'open',        'low',    3)")

  # --- objectives
  DBI::dbExecute(con, "INSERT INTO objectives
    (organization_id, title, description, owner_id, status, progress, deadline) VALUES
    (1, 'Atteindre 50 clients',    'Croissance Q2',           1, 'active', 42, '2025-06-30'),
    (1, 'Lancer Mobile App',       'iOS + Android MVP',       2, 'active', 40, '2025-06-15'),
    (1, 'ARR 500k€',               'Objectif annuel',         1, 'active', 28, '2025-12-31')")

  # --- kpis
  DBI::dbExecute(con, "INSERT INTO kpis
    (organization_id, name, value, target, unit, period) VALUES
    (1, 'Monthly Revenue',   12450, 20000, 'EUR', '2025-03'),
    (1, 'Active Customers',     28,    50, 'count', '2025-03'),
    (1, 'CAC',                 180,   150, 'EUR', '2025-03'),
    (1, 'Churn',               2.1,   1.5, '%',   '2025-03')")

  # --- activities
  DBI::dbExecute(con, "INSERT INTO activities
    (organization_id, user_id, type, description, entity_type) VALUES
    (1, 1, 'project_created', 'Project ''API Integration'' created', 'project'),
    (1, 2, 'task_completed',  'Task ''Setup React Native project'' completed', 'task'),
    (1, 3, 'customer_added',  'Customer ''Umbrella Labs'' added', 'customer'),
    (1, 1, 'opportunity_updated', 'Opportunity ''Northwind - Expansion'' moved to Proposal', 'opportunity'),
    (1, 2, 'task_created',    'Task ''Auth screens'' created', 'task'),
    (1, 1, 'project_updated', 'Project ''Website Redesign'' progress 65%', 'project')")

  # --- notifications
  DBI::dbExecute(con, "INSERT INTO notifications
    (organization_id, user_id, type, title, message) VALUES
    (1, 1, 'task_due',   '2 tâches arrivent à échéance', 'Voir les tâches'),
    (1, 1, 'lead_new',   'Nouveau lead entrant',         'Vandelay Industries')")

  # revenue history (for chart) - on stocke dans activities pour simplicité démo
  invisible()
}

migrate_auth_schema <- function(pool) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  fields <- DBI::dbListFields(con, "users")
  additions <- c(
    role = "TEXT DEFAULT 'member'",
    password_hash = "TEXT",
    failed_attempts = "INTEGER NOT NULL DEFAULT 0",
    locked_until = "TEXT",
    last_login_at = "TEXT"
  )
  for (field in names(additions)) {
    if (!field %in% fields) {
      DBI::dbExecute(con, sprintf("ALTER TABLE users ADD COLUMN %s %s", field, additions[[field]]))
    }
  }
  DBI::dbExecute(con, "UPDATE users SET role = CASE role_id WHEN 1 THEN 'admin' WHEN 2 THEN 'owner' WHEN 3 THEN 'member' ELSE role END WHERE role_id IS NOT NULL")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_users_email_unique ON users(lower(email))")
}