#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. See
#'   \code{\link[golem]{get_golem_options}} for more details.
#'
#' @export
run_app <- function(...) {
  golem::with_golem_options(
    app = .bookclub_app(),
    golem_opts = list(...)
  )
}

.bookclub_app <- function() {
  shinyslack::shinyslack_app(
    ui = .app_ui,
    server = .app_server,
    team_id = .team_id,
    enableBookmarking = "url",
    onStart = .app_global
  )
}
