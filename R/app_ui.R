#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @keywords internal
.app_ui <- function(request) {
  shiny::tagList(
    .golem_add_external_resources(),
    shiny::fluidPage(
      shiny::titlePanel("R4DS Book Club Planner"),
      shiny::fluidRow(
        shiny::column(width = 2, .user_ui()),
        shiny::column(width = 2, .book_ui()),
        shiny::column(width = 2, .timezone_ui()),
        shiny::column(
          width = 2,
          offset = 1,
          shiny::p(shiny::strong("Finish by submitting")),
          shiny::actionButton(inputId = "submit", label = "Submit")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4("Select your availability"),
          shiny::p(
            "(missing check boxes are unavailable; other R4DS clubs are using the
            Zoom account at those times; colored cells indicate existing sign ups)"),
          .calendar_ui()
        )
      )
    )
  )
}
