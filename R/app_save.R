#' Save User Selections
#'
#' @param user_name The name of this user.
#' @param user_id The Slack id of this user.
#' @param book_name The name of the book for which they're choosing times.
#' @param timezone The user's timezone.
#' @param time_table The user's selections.
#'
#' @return The sheet ID, invisibly.
#' @keywords internal
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

  # Unselect any illegal things they've managed to select, and set any NAs to
  # FALSE.
  unavailable_times <- .get_unavailable_times(timezone)

  selected_times <- rhandsontable::hot_to_r(time_table) |>
    dplyr::mutate(hour = 0:23) |>
    tidyr::pivot_longer(
      cols = .data$Monday:.data$Sunday,
      names_to = "day",
      values_to = "available"
    ) |>
    dplyr::left_join(unavailable_times, by = c("day", "hour")) |>
    dplyr::mutate(
      unavailable = tidyr::replace_na(.data$unavailable, FALSE),
      available = .data$available & !.data$unavailable,
      available = tidyr::replace_na(.data$available, FALSE)
    ) |>
    dplyr::select(-"unavailable")

  user_availability_df <- data.frame(
    user_name = user_name,
    user_id = user_id,
    book_name = book_name,
    timezone = timezone,
    submission_timestamp = as.character(Sys.time())
  ) |>
    dplyr::bind_cols(
      selected_times
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
    sheet = "Signups"
  )
}
