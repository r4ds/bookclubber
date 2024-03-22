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
    user_info <- .bookclubber_user_info()
    output$user_name <- renderText(
      paste(
        strong("Logged in as"),
        br(),
        user_info()[["user_name"]]
      )
    )
    return(user_info)
  })
}

.bookclubber_user_info <- function() {
  slack_user_info <- .shinyslack_user_info(
    c("user_id", "display_name", "real_name", "user_name")
  )
  return(
    shiny::reactive({
      list(
        user_id = slack_user_info()[["user_id"]],
        user_name = slack_user_info()[["display_name"]] %|"|%
          slack_user_info()[["real_name"]] %|"|%
          slack_user_info()[["user_name"]]
      )
    })
  )
}

# Abstract for mocking.
.shinyslack_user_info <- function(components) {
  shinyslack::user_info(components) # nocov
}

`%|"|%` <- function(x, y) {
  if (is.null(x) || !isTRUE(as.logical(nchar(x)))) {
    return(y)
  }
  return(x)
}
