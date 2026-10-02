mod_activities_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page-header",
      h2("Activity Center", class = "page-title"),
      p("Historique des actions", class = "page-subtitle")
    ),
    div(class = "card",
      actionButton(ns("refresh"), tagList(icon("rotate"), "Refresh"),
                   class = "btn btn-default btn-sm"),
      uiOutput(ns("timeline"))
    )
  )
}

mod_activities_server <- function(id, actor) {
  moduleServer(id, function(input, output, session) {
    refresh <- reactiveVal(0)
    observeEvent(input$refresh, refresh(refresh() + 1))

    output$timeline <- renderUI({
      refresh()
      df <- get_activities(actor(), limit = 50)
      if (nrow(df) == 0) return(empty_state("Aucune activité"))
      lapply(seq_len(nrow(df)), function(i) {
        div(class = "timeline-row",
          div(class = "timeline-marker"),
          div(class = "timeline-body",
            div(class = "timeline-title", df$description[i]),
            div(class = "timeline-meta",
              tags$span(class = "tag-type", df$type[i]),
              " · ",
              df$user_name[i], " · ", df$created_at[i]
            )
          )
        )
      })
    })
  })
}