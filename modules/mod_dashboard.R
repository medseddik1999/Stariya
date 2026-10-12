mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page-header dashboard-heading",
      div(
        div(class = "eyebrow", "STARTUP OVERVIEW"),
        h2("Dashboard", class = "page-title"),
        p("Vue d'ensemble de votre startup", class = "page-subtitle")
      ),
      div(class = "dashboard-period",
        icon("calendar"),
        span(format(Sys.Date(), "%b %Y"))
      )
    ),

    fluidRow(
      column(3, kpi_card("Revenue", ns("kpi_revenue"), icon_name = "sack-dollar", variant = "revenue")),
      column(3, kpi_card("Active Customers", ns("kpi_customers"), icon_name = "users", variant = "customers")),
      column(3, kpi_card("Active Projects", ns("kpi_projects"), icon_name = "folder-open", variant = "projects")),
      column(3, kpi_card("Open Tasks", ns("kpi_tasks"), icon_name = "list-check", variant = "tasks"))
    ),

    fluidRow(
      column(8,
        div(class = "card dashboard-panel revenue-panel",
          div(class = "panel-heading",
            div(
              div(class = "panel-title", "Opportunity Pipeline"),
              div(class = "panel-subtitle", "Amount by opportunity")
            ),
            span(class = "panel-period", "Current pipeline")
          ),
          plotly::plotlyOutput(ns("revenue_chart"), height = "260px")
        )
      ),
      column(4,
        div(class = "card dashboard-panel progress-panel",
          div(class = "panel-heading",
            div(
              div(class = "panel-title", "Project Progress"),
              div(class = "panel-subtitle", "Active projects")
            )
          ),
          uiOutput(ns("project_progress"))
        )
      )
    ),

    fluidRow(
      column(7,
        div(class = "card dashboard-panel",
          div(class = "panel-heading",
            div(
              div(class = "panel-title", "Recent Activity"),
              div(class = "panel-subtitle", "Latest updates across your workspace")
            )
          ),
          uiOutput(ns("recent_activity"))
        )
      ),
      column(5,
        div(class = "card dashboard-panel",
          div(class = "panel-heading",
            div(
              div(class = "panel-title", "Upcoming Tasks"),
              div(class = "panel-subtitle", "Deadlines to keep in view")
            )
          ),
          uiOutput(ns("upcoming_tasks"))
        )
      )
    )
  )
}

mod_dashboard_server <- function(id, actor) {
  moduleServer(id, function(input, output, session) {

    stats <- reactive({
      dashboard_stats(actor())
    })

    output$kpi_revenue   <- renderText(format_currency(stats()$revenue))
    output$kpi_customers <- renderText(stats()$customers)
    output$kpi_projects  <- renderText(stats()$projects)
    output$kpi_tasks     <- renderText(stats()$open_tasks)

    output$revenue_chart <- plotly::renderPlotly({
      df <- opportunity_series(actor())
      if (nrow(df) == 0) {
        return(plotly::plotly_empty(type = "bar", orientation = "h") %>%
          plotly::layout(
            title = list(text = "Aucune opportunité pour le moment", font = list(family = "Inter", size = 13, color = "#64748B")),
            paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF"
          ))
      }
      plotly::plot_ly(df, x = ~value, y = ~name, type = "bar", orientation = "h",
                      marker = list(
                        color = "#6366F1",
                        line = list(color = "#4F46E5", width = 1),
                        cornerradius = 4
                      ),
                      customdata = ~stage,
                      hovertemplate = paste0(
                        "<b>%{y}</b><br>Montant: €%{x:,.0f}",
                        "<br>Étape: %{customdata}<extra></extra>"
                      )) %>%
        plotly::layout(
          margin = list(t = 10, b = 28, l = 10, r = 16),
          font = list(family = "Inter, sans-serif", size = 12, color = "#475569"),
          xaxis = list(title = "", tickprefix = "€", showgrid = TRUE,
                       gridcolor = "#F1F5F9", zeroline = FALSE),
          yaxis = list(title = "", autorange = "reversed", showgrid = FALSE),
          plot_bgcolor  = "#FFFFFF",
          paper_bgcolor = "#FFFFFF"
        )
    })

    output$project_progress <- renderUI({
      df <- get_projects(actor()) %>% dplyr::filter(status == "in_progress") %>% head(6)
      if (nrow(df) == 0) return(empty_state("Aucun projet actif"))
      lapply(seq_len(nrow(df)), function(i) {
        pct <- df$progress[i]
        bar_color <- if (pct >= 80) "linear-gradient(90deg, #10B981, #059669)" else if (pct >= 40) "linear-gradient(90deg, #6366F1, #4F46E5)" else "linear-gradient(90deg, #F59E0B, #D97706)"
        div(class = "progress-row",
          div(class = "progress-top",
            span(class = "progress-name", df$name[i]),
            span(class = "progress-pct",  paste0(pct, "%"))
          ),
          div(class = "progress-bar",
            div(class = "progress-fill", style = sprintf("width:%s%%; background: %s;", pct, bar_color))
          )
        )
      })
    })

    output$recent_activity <- renderUI({
      df <- get_activities(actor(), limit = 6)
      if (nrow(df) == 0) return(empty_state("Aucune activité récente"))
      lapply(seq_len(nrow(df)), function(i) {
        div(class = "activity-row",
          div(class = "activity-dot-container",
            div(class = "activity-dot")
          ),
          div(class = "activity-body",
            div(class = "activity-title", df$description[i]),
            div(class = "activity-meta",
              tags$span(class = "activity-user", icon("user"), df$user_name[i]),
              tags$span(class = "activity-time", icon("clock"), df$created_at[i])
            )
          )
        )
      })
    })

    output$upcoming_tasks <- renderUI({
      df <- get_tasks(actor()) %>%
        dplyr::filter(status != "done", !is.na(due_date)) %>%
        dplyr::arrange(due_date) %>% head(6)
      if (nrow(df) == 0) return(empty_state("Aucune tâche à venir"))
      lapply(seq_len(nrow(df)), function(i) {
        div(class = "task-row",
          div(class = "task-main", df$title[i]),
          div(class = "task-meta",
            tags$span(class = "task-project", icon("folder"), df$project_name[i] %||% "Général"),
            tags$span(class = "task-date", icon("calendar-days"), format_date(df$due_date[i]))
          )
        )
      })
    })
  })
}