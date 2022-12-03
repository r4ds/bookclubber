#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @keywords internal
.app_server <- function(input, output, session) {
  # I need the user info outside of the username thing, so for now I collect it
  # separately. Ideally it should be more strongly associated with the user
  # code.
  slack_user_info <- shinyslack::user_info()

  # Update and otherwise deal with the simple inputs across the top.
  .user_server(slack_user_info = slack_user_info)
  .book_server() # Should become fully module-ized later.
  .timezone_server()

  # Display the week calendar. I haven't made this a module yet since it's
  # MOSTLY just rhandsontable, but it will probably convert soonish.
  output$time_table <- .calendar_selector()

  # Save their selections on Submit.
  shiny::observeEvent(
    input$submit,
    .save_availability(
      slack_user_info()[["user_name"]],
      slack_user_info()[["user_id"]],
      input$book_name,
      input[[shiny::NS("timezone", "selected_zone")]],
      input$time_table
    )
  )
}

#' Read a Sheet from the GS4 Workbook
#'
#' @param ... Arguments passed on to [googlesheets4::read_sheet()].
#'
#' @return A google sheet.
#' @keywords internal
.read_gs4 <- function(...) {
  googlesheets4::read_sheet(.gs4_sheet_id, ...)
}
