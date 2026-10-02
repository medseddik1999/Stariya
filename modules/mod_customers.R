mod_customers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page-header",
      div(
        h2("Customers", class = "page-title"),
        p("Base clients et fiches détaillées", class = "page-subtitle")
      ),
      actionButton(ns("new_customer"), tagList(icon("plus"), "New Customer"),
                   class = "btn btn-primary btn-sm")
    ),
    div(class = "card", DTOutput(ns("customers_table")))
  )
}

mod_customers_server <- function(id, actor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    output$customers_table <- renderDT({
      refresh()
      df <- get_customers(actor())
      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No customers yet."), rownames = FALSE,
                         options = list(dom = 't')))
      }
      df_display <- data.frame(
        Company = df$company_name,
        Contact = df$contact_name %||% "—",
        Email   = df$email %||% "—",
        Status  = status_badge(df$status),
        Segment = df$segment %||% "—",
        Actions = action_buttons(ns, df$id),
        stringsAsFactors = FALSE
      )
      datatable(df_display, escape = FALSE, rownames = FALSE,
                class = "stripe hover compact",
                options = list(dom = 'ft', pageLength = 10))
    }, server = TRUE)

    open_modal <- function(customer = NULL) {
      is_edit <- !is.null(customer)
      showModal(modalDialog(
        title = if (is_edit) "Edit customer" else "New customer",
        easyClose = TRUE, size = "m",
        textInput(ns("c_company"), "Company", value = if (is_edit) customer$company_name else ""),
        textInput(ns("c_contact"), "Contact name", value = if (is_edit) customer$contact_name %||% "" else ""),
        textInput(ns("c_email"),   "Email", value = if (is_edit) customer$email %||% "" else ""),
        textInput(ns("c_phone"),   "Phone", value = if (is_edit) customer$phone %||% "" else ""),
        fluidRow(
          column(6, selectInput(ns("c_status"), "Status",
            choices = c("active","prospect","inactive"),
            selected = if (is_edit) customer$status else "active")),
          column(6, textInput(ns("c_segment"), "Segment",
            value = if (is_edit) customer$segment %||% "" else ""))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_customer"), if (is_edit) "Save" else "Create",
                       class = "btn btn-primary")
        )
      ))
      session$userData$editing_customer_id <- if (is_edit) customer$id else NULL
    }

    observeEvent(input$new_customer, open_modal())
    observeEvent(input$edit_id, {
      c <- get_customer(input$edit_id, actor())
      if (nrow(c) == 1) open_modal(c)
    })

    observeEvent(input$save_customer, {
      data <- list(
        company_name = trimws(input$c_company),
        contact_name = trimws(input$c_contact),
        email        = trimws(input$c_email),
        phone        = trimws(input$c_phone),
        status       = input$c_status,
        segment      = trimws(input$c_segment)
      )
      if (!nzchar(data$company_name)) {
        showNotification("Le nom de la société est obligatoire.", type = "error"); return()
      }
      tryCatch({
        eid <- session$userData$editing_customer_id
        if (is.null(eid)) {
          create_customer(data, actor()); showNotification("Customer created successfully.", type = "message")
        } else {
          update_customer(eid, data, actor()); showNotification("Customer updated successfully.", type = "message")
        }
        session$userData$editing_customer_id <- NULL
        removeModal(); refresh(refresh() + 1)
      }, error = function(e) {
        message("customer save error: ", e$message)
        showNotification("Unable to save the customer.", type = "error")
      })
    })

    observeEvent(input$delete_id, {
      showModal(modalDialog(
        title = "Delete customer",
        "Are you sure?",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_customer"), "Delete", class = "btn btn-danger")
        )
      ))
      session$userData$deleting_customer_id <- input$delete_id
    })

    observeEvent(input$confirm_delete_customer, {
      id <- session$userData$deleting_customer_id
      if (!is.null(id)) {
        tryCatch({
          delete_customer(id, actor())
          showNotification("Customer deleted.", type = "message")
          refresh(refresh() + 1)
        }, error = function(e) showNotification("Unable to delete customer.", type = "error"))
      }
      session$userData$deleting_customer_id <- NULL
      removeModal()
    })
  })
}