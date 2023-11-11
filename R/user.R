# Most or all of this should eventually move to r4ds/shinyslack.

#' User UI Side
#'
#' @inheritParams .shared-parameters
#'
#' @inherit shiny::htmlOutput return
#' @keywords internal
.user_ui <- function(id = "user_name") {
  shiny::htmlOutput(outputId = shiny::NS(id, "user_name"))
}

#' User Server Side
#'
#' @inheritParams .shared-parameters
#'
#' @return A moduleServer object.
#' @keywords internal
.user_server <- function(id = "user_name") {
  shiny::moduleServer(id, function(input, output, session) {
    slack_user_info <- .shinyslack_user_info(c("display_name", "user_id"))
    output$user_name <- shiny::renderText(
      paste(
        shiny::strong("Logged in as"),
        shiny::br(),
        slack_user_info()[["display_name"]]
      )
    )
    return(slack_user_info)
  })
}

# Abstract for mocking.
.shinyslack_user_info <- function(components) {
  shinyslack::user_info(components) # nocov
}
