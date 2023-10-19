.submit_availability <- function(user_name,
                                 user_id,
                                 selected_book,
                                 user_timezone,
                                 time_table) {
  if (selected_book == "") {
    return(
      .modal_fixit(error = "No book selected", action = "Please select a book.")
    )
  }
  if (is.null(time_table)) {
    return(
      .modal_fixit(
        error = "No availability",
        action = "Please wait for the availability picker to load."
      )
    )
  }
  return(
    .save_availability(
      user_name,
      user_id,
      selected_book,
      user_timezone,
      time_table
    )
  )
}

.save_availability <- function(user_name,
                               user_id,
                               selected_book,
                               user_timezone,
                               time_table) {
  .modal_wait(
    "Submitting availability",
    "Saving availability to the cloud. Please do not close this window."
  )
  .save_availability_impl(
    user_name,
    user_id,
    selected_book,
    user_timezone,
    time_table
  )
  shiny::removeModal()
}

.save_availability_impl <- function(user_name,
                                    user_id,
                                    selected_book,
                                    user_timezone,
                                    time_table) {
  selected_times <- .time_table_process(time_table, user_timezone)
  if (!nrow(selected_times)) {
    return(bookclubdata::signups_clear_user_book(user_id, selected_book))
  }
  .user_availabity_save(
    selected_times,
    user_name,
    user_id,
    user_timezone,
    selected_book
  )
}

.user_availabity_save <- function(selected_times,
                                  user_name,
                                  user_id,
                                  user_timezone,
                                  selected_book) {
  bookclubdata::signups_write(
    .user_availability_prepare(
      selected_times,
      user_name,
      user_id,
      user_timezone
    ),
    book_name = selected_book
  )
}

.user_availability_prepare <- function(selected_times,
                                       user_name,
                                       user_id,
                                       user_timezone) {
  user_availability_df <- dplyr::bind_cols(
    data.frame(
      user_id = user_id,
      submission_timestamp = .pretty_now(),
      user_name = user_name
    ),
    .user_availability_to_utc(selected_times, user_timezone)
  )

  return(user_availability_df)
}

.user_availability_to_utc <- function(selected_times, user_timezone) {
  selected_times$timezone <- user_timezone
  selected_times$datetime_utc <- bookclubdata::make_datetimes_utc(
    selected_times$day, selected_times$hour, selected_times$timezone
  )
  selected_times$day <- NULL
  selected_times$hour <- NULL
  return(selected_times)
}
