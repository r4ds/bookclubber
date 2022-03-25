#' Run the Shiny Application
#'
#' @param local Logical indicating whether to run the app locally or on a
#'   server. Useful for debugging.
#' @param ... arguments to pass to golem_opts. See
#'   \code{\link[golem]{get_golem_options}} for more details.
#'
#' @export
run_app <- function(local = interactive(), ...) {
  if (local) {
    site_url <- "http://127.0.0.1:4242/"
    options <- list(
      port = 4242L,
      launch.browser = TRUE
    )
  } else {
    site_url <- .site_url
    options <- list()
  }
  app <- shiny::shinyApp(
    ui = shinyslack::slack_shiny_ui(
      ui = .app_ui,
      team_id = .team_id,
      site_url = site_url
    ),
    server = .app_server,
    onStart = .app_global,
    options = options,
    enableBookmarking = "url"
  )

  golem::with_golem_options(
    app = app,
    golem_opts = list(...)
  )
}
