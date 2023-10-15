# Functions for manual processing of the data, setting up new clubs, and that
# sort of thing.

#' Pick a Convenient Time
#'
#' There's a lot of work to do on this still.
#'
#' @param book_name The abbreviation for an approved book, such as rpkgs or
#'   mshiny.
#' @param facilitator_id The Slack ID of the facilitator for this group.
#'   Available via the user's "full profile" in Slack.
#' @param required_choosers The minimum number of sign-ups necessary for the
#'   club to launch. Think really hard before changing this.
#'
#' @return The best times, as a tibble.
#' @export
choose_time <- function(book_name,
                        facilitator_id,
                        required_choosers = 5) {
  # Verify that book_name is in our list.
  # book_name <- rlang::arg_match(book_name, values = approved_books)

  df <- .load_signups(.unavailable_times()$unavailable_time) |>
    dplyr::filter(.data$book_name == .env$book_name) |>
    dplyr::select(-"book_name")

  facilitator_times <- df |>
    dplyr::filter(.data$user_id == facilitator_id)

  if (!nrow(facilitator_times)) {
    rlang::abort(
      "That facilitator has not chosen any valid times for that book."
    )
  }

  facilitator_tz <- facilitator_times$timezone[[1]]

  best_times <- df |>
    dplyr::filter(
      .data$datetime_utc %in% facilitator_times$datetime_utc
    ) |>
    dplyr::mutate(user_name = glue::glue("@{.data$user_name}")) |>
    dplyr::group_by(.data$datetime_utc) |>
    dplyr::summarize(
      n = dplyr::n(),
      users = glue::glue_collapse(sort(unique(.data$user_name)), sep = ", ")
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))

  facilitator_minutes <- .tz_minutes(facilitator_tz, df$datetime_utc[[1]])

  if (facilitator_minutes > 0) {
    cli::cli_alert_warning(
      "Facilitator timezone has a {facilitator_minutes} minute offset."
    )
  }

  if (max(best_times$n) < required_choosers) {
    withr::local_options(cli.condition_width = Inf)
    user_tags <- paste0("@", sort(unique(df$user_name)))
    starter_msg <- glue::glue(
      ": You have all indicated interest in joining this cohort to read {book_name}, but we",
      "don't yet have enough people with overlappping schedules to launch.",
      .sep = " "
    )
    general_message <- "@here: If you're in this channel, you must have some interest in joining a cohort."
    book_esc <- utils::URLencode(book_name)
    book_url <- glue::glue("https://r4ds.io/bookclubber?bookname={book_esc}")
    after_app <- glue::glue(
      "especially any green times (the shading indicates how many people have chosen that time).",
      "I'll check again next Monday to see if we are ready to launch!",
      .sep = " "
    )
    for_me <- glue::glue("choose_time(\"{book_name}\", \"{facilitator_id}\")")
    cli::cli_inform(
      c(
        "!" = "No times with {required_choosers}+ users.",
        "",
        "{user_tags}{starter_msg}",
        "",
        "{general_message} Please check the app ({book_url}) -- {after_app}",
        "",
        "For me: This is the code to check this club again:",
        "{.code {for_me}}"
      )
    )
  } else {
    clean_times <- best_times |>
      dplyr::filter(.data$n >= required_choosers) |>
      dplyr::mutate(
        datetime_r4ds = lubridate::with_tz(
          .data$datetime_utc, "America/Chicago"
        ),
        datetime_facilitator = lubridate::with_tz(
          .data$datetime_utc, facilitator_tz
        ),
        day_you = lubridate::wday(
          .data$datetime_facilitator, label = TRUE, abbr = FALSE
        ),
        hour_you = lubridate::hour(.data$datetime_facilitator),
        day_r4ds = lubridate::wday(
          .data$datetime_r4ds, label = TRUE, abbr = FALSE
        ),
        hour_r4ds = lubridate::hour(.data$datetime_r4ds)
      ) |>
      dplyr::select(
        "n",
        "day_you",
        "hour_you",
        "day_r4ds",
        "hour_r4ds",
        "users"
      )

    minutes_you <- stringr::str_pad(facilitator_minutes, 2, pad = "0")

    cli::cli_bullets(
      glue::glue_data(
        clean_times,
        "{n} people: {day_you}s {hour_you}:{minutes_you} ({users})",
        "({day_r4ds}s {hour_r4ds}:00 R4DS time)",
        .sep = "\n"
      )
    )

    return(invisible(clean_times))
  }
}
