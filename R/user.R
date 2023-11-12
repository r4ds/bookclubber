# Most or all of this should eventually move to r4ds/shinyslack.

#' User UI Side
#'
#' @inheritParams .shared-parameters
#'
#' @inherit shiny::htmlOutput return
#' @keywords internal
.user_ui <- function(id = "user_name") {
  htmlOutput(outputId = NS(id, "user_name"))
}

#' User module server
#'
#' @inheritParams .shared-parameters
#'
#' @return The user's `display_name` and `user_id` in a reactive list.
#' @keywords internal
.user_server <- function(id = "user_name") {
  moduleServer(id, function(input, output, session) {
    slack_user_info <- .shinyslack_user_info(c("display_name", "user_id"))
    output$user_name <- renderText(
      paste(
        strong("Logged in as"),
        br(),
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
