#' Run the Shiny Application
#'
#' @param local Logical indicating whether to run the app locally or on a
#'   server. Useful for debugging.
#' @param ... arguments to pass to golem_opts. See
#'   \code{\link[golem]{get_golem_options}} for more details.
#'
#' @export
run_app <- function(local = FALSE, ...) {
  if (local) {
    options <- list(
      port = 4242L,
      launch.browser = TRUE
    )
  } else {
    options <- list()
  }
  app <- shiny::shinyApp(
    ui = .app_ui,
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
