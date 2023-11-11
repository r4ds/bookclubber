.modal_fixit <- function(error, action) {
  showModal(
    modalDialog(
      action,
      title = paste("Error:", error),
      easyClose = TRUE,
      footer = tagList(modalButton("Ok"))
    )
  )
}

.modal_wait <- function(action, message) {
  showModal(
    modalDialog(
      message,
      title = paste("Please wait:", action),
      footer = NULL,
      fade = FALSE
    )
  )
}
