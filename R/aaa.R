#' Parameters used in multiple functions
#'
#' These are parameters that are likely to show up in multiple functions, so I
#' gather their definitions in one easy-to-find place.
#'
#' @param id The identifier of this input.
#' @param session The shiny session. I don't know of a case where we shouldn't
#'   use the default.
#' @param user_id Slack's unique id for this user.
#' @param user_name The Slack display name of this user.
#' @param user_timezone The timezone currently chosen by the user (as a
#'   character scalar).
#' @param selected_book The book currently selected by this user (as a character
#'   scalar).
#' @param signups A dataframe of signup information, converted to UTC.
#'
#' @name .shared-parameters
#' @keywords internal
NULL
