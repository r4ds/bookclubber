#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @keywords internal
.app_ui <- function(request) {
  tagList(
    .golem_add_external_resources(),
    .ui_page()
  )
}

.ui_page <- function() {
  fluidPage(
    titlePanel("R4DS Book Club Planner"),
    .ui_settings(),
    .ui_calendar_row()
  )
}

.ui_settings <- function() {
  fluidRow(
    column(2, .user_ui()),
    column(2, .timezone_ui()),
    column(2, .book_ui()),
    column(2, .submit_ui(), offset = 1)
  )
}
