`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a)) || identical(a, "")) b else a

format_currency <- function(x, symbol = "€") {
  if (is.null(x) || length(x) == 0) return(paste0(symbol, "0"))
  paste0(symbol, formatC(as.numeric(x), format = "f", digits = 0, big.mark = ","))
}

format_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return("—")
  tryCatch(format(as.Date(x), "%b %d, %Y"), error = function(e) "—")
}

status_badge <- function(status) {
  s <- tolower(status %||% "")
  cls <- dplyr::case_when(
    s %in% c("active","done","won","completed","closed") ~ "badge-success",
    s %in% c("in_progress","review","contacted")          ~ "badge-info",
    s %in% c("planning","todo","draft","prospect","new")  ~ "badge-muted",
    s %in% c("on_hold","at_risk","qualified","proposal")  ~ "badge-warning",
    s %in% c("cancelled","lost","inactive")               ~ "badge-danger",
    TRUE ~ "badge-muted"
  )
  sprintf('<span class="badge %s">%s</span>', cls, status)
}

priority_badge <- function(priority) {
  p <- tolower(priority %||% "")
  cls <- dplyr::case_when(
    p %in% c("high","urgent") ~ "badge-danger",
    p == "medium"             ~ "badge-warning",
    p == "low"                ~ "badge-muted",
    TRUE ~ "badge-muted"
  )
  sprintf('<span class="badge %s">%s</span>', cls, priority)
}

action_buttons <- function(ns, ids) {
  vapply(ids, function(id) {
    sprintf(
      '<div class="action-buttons">
         <button class="btn-action" title="Edit"
           onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'})">
           <i class="fa fa-pen"></i>
         </button>
         <button class="btn-action btn-action-danger" title="Delete"
           onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'})">
           <i class="fa fa-trash"></i>
         </button>
       </div>',
      ns("edit_id"), id, ns("delete_id"), id
    )
  }, character(1))
}

kpi_card <- function(label, value_id, delta = NULL, icon_name = "chart-line", variant = "default") {
  div(class = "kpi-card",
    div(class = "kpi-topline",
      div(class = "kpi-label", label),
      div(class = paste("kpi-icon", paste0("kpi-icon-", variant)), icon(icon_name))
    ),
    div(class = "kpi-value", textOutput(value_id, inline = TRUE)),
    if (!is.null(delta)) div(class = "kpi-delta", icon("arrow-trend-up"), span(delta), " vs last month")
  )
}

empty_state <- function(msg = "Nothing here yet.") {
  div(class = "empty-state", icon("inbox"), tags$p(msg))
}