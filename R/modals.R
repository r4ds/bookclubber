.modal_fixit <- function(error, action) {
  shiny::showModal(
    shiny::modalDialog(
      action,
      title = paste("Error:", error),
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton("Ok"))
    )
  )
}

.modal_wait <- function(action, message) {
  shiny::showModal(
    shiny::modalDialog(
      message,
      title = paste("Please wait:", action),
      footer = NULL,
      fade = FALSE
    )
  )
}
