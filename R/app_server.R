#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @keywords internal
.app_server <- function(input, output, session) {
  slack_user_info <- .user_server()

  approved_books <- bookclubdata::approved_books(refresh = TRUE)
  signups <- shiny::reactive({
    .load_book_signups(input$book_name)
  })

  # Update and otherwise deal with the simple inputs across the top.
  # Should become fully module-ized later.
  .book_observer(approved_books = approved_books)
  .timezone_server()

  # I need to sort out the reactivity to make this its own module. For now this
  # works and other attempts didn't.
  shiny::observe({
    shiny::req(
      input[[shiny::NS("timezone", "selected_zone")]],
      approved_books,
      signups(),
      slack_user_info()[["user_id"]]
    )
    output[[shiny::NS("calendar", "availability")]] <- .calendar_selector(
      slack_user_info()[["user_id"]],
      input[[shiny::NS("timezone", "selected_zone")]],
      signups()
    )
  })

  shiny::observeEvent(
    input$submit,
    .submit_availability(
      slack_user_info()[["display_name"]],
      slack_user_info()[["user_id"]],
      input$book_name,
      input[[shiny::NS("timezone", "selected_zone")]],
      input[[shiny::NS("calendar", "availability")]]
    ),
    priority = 1000
  )
}
