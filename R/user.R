#' User UI Side
#'
#' @inheritParams .shared-parameters
#'
#' @return A shiny module.
#' @keywords internal
.user_ui <- function(id = "user_name") {
  return(
    shiny::htmlOutput(outputId = shiny::NS(id, "user_name"))
  )
}

#' User Server Side
#'
#' @inheritParams .shared-parameters
#' @param slack_user_info The object that contains info from Slack.
#'
#' @return A moduleServer object.
#' @keywords internal
.user_server <- function(id = "user_name", slack_user_info) {
  shiny::moduleServer(id, function(input, output, session) {
    output$user_name <- shiny::renderText(
      paste(
        shiny::strong("Logged in as"),
        shiny::br(),
        slack_user_info()[["display_name"]]
      )
    )
  })
}
