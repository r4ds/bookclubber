% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendar.R
\name{.hour_to_name}
\alias{.hour_to_name}
\title{Create rownames from hours}
\usage{
.hour_to_name(hour, user_timezone)
}
\arguments{
\item{hour}{An integer vector of hours. This will almost certainly just be
0:23 repeated 7 times.}

\item{user_timezone}{The timezone currently chosen by the user (as a
character scalar).}
}
\value{
A character vector the same length as hour, with formatted times.
}
\description{
Create rownames from hours
}
\keyword{internal}
