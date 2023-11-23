#' The application server-side
#'
#' @param input,output,session Internal parameters for shiny.
#' @keywords internal
.app_server <- function(input, output, session) {
  # Set up input row.
  cli::cli_inform("Setting up input row.")
  slack_user_info <- .user_server()
  timezone <- .timezone_server()
  selected_book <- .book_server()

  cli::cli_inform("Loading signups.")
  signups <- reactive({
    .load_book_signups(selected_book())
  })

  # I need to sort out the reactivity to make this its own module. For now this
  # works and other attempts didn't.
  cli::cli_inform("Setting up calendar observer.")
  observe({
    req(
      timezone(),
      !is.null(selected_book()),
      selected_book() != "",
      signups(),
      slack_user_info()[["user_id"]]
    )
    output[[NS("calendar", "availability")]] <- .calendar_selector(
      slack_user_info()[["user_id"]],
      timezone(),
      signups()
    )
  })

  cli::cli_inform("Setting up submit observer.")
  observeEvent(
    input$submit,
    .submit_availability(
      slack_user_info()[["display_name"]],
      slack_user_info()[["user_id"]],
      selected_book(),
      timezone(),
      input[[NS("calendar", "availability")]]
    ),
    priority = 1000
  )
}
