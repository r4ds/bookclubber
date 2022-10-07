#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. See
#'   \code{\link[golem]{get_golem_options}} for more details.
#'
#' @export
run_app <- function(...) {
  app <- shinyslack::shinyslack_app(
    ui = scenes::add_cookie_javascript(.app_ui),
    server = .app_server,
    team_id = .team_id,
    site_url = .site_url,
    enableBookmarking = "url",
    onStart = .app_global
  )

  golem::with_golem_options(
    app = app,
    golem_opts = list(...)
  )
}
