#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @keywords internal
.app_ui <- function(request) {
  shiny::tagList(
    .golem_add_external_resources(),
    .ui_page()
  )
}

.ui_page <- function() {
  shiny::fluidPage(
    shiny::titlePanel("R4DS Book Club Planner"),
    .ui_settings(),
    .ui_calendar_row()
  )
}

.ui_settings <- function() {
  shiny::fluidRow(
    shiny::column(2, .user_ui()),
    shiny::column(2, .book_ui()),
    shiny::column(2, .timezone_ui()),
    shiny::column(2, .submit_ui(), offset = 1)
  )
}
