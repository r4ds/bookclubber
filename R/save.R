#' Save User Selections
#'
#' @param user_info A named character vector
#' @param book_name
#' @param timezone
#' @param time_table
#'
#' @return
#' @export
#'
#' @examples
.save_availability <- function(user_name,
                               user_id,
                               book_name,
                               timezone,
                               time_table) {
  # Show a modal right away to prevent further submits.
  shiny::showModal(
    shiny::modalDialog(
      title = "Thank you, your availability has been submitted.",
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton("Ok"))
    )
  )

  user_availability_df <- data.frame(
    user_name = user_name,
    user_id = user_id,
    book_name = book_name,
    timezone = timezone,
    submission_timestamp = as.character(Sys.time())
  ) |>
    dplyr::bind_cols(
      rhandsontable::hot_to_r(time_table)
    ) |>
    dplyr::mutate(hour = 0:23) |>
    tidyr::pivot_longer(
      cols = .data$Monday:.data$Sunday,
      names_to = "day",
      values_to = "available"
    ) |>
    dplyr::select(
      "user_name",
      "user_id",
      "book_name",
      "timezone",
      "submission_timestamp",
      "day",
      "hour",
      "available"
    )

  googlesheets4::sheet_append(
    .gs4_sheet_id,
    user_availability_df,
    sheet = 1
  )
}
