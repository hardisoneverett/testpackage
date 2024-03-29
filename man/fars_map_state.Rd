% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packageproject.R
\name{fars_map_state}
\alias{fars_map_state}
\title{fars_map_state}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{A Integer}

\item{year}{Integer}
}
\value{
A map with state lines and accident locations (mapped as points).
If there is no data to plot, then that message will be shown.
}
\description{
This function takes in a number representing a state and a year.  The
function outputs a map with the state's lines and accident locations.
If the \code{state.num} is not in the data then an error is returned. If
the file cannot be found or is in the incorrect format and error will also
be shown.
}
\examples{
\dontrun{fars_map_state(9,2013)}

}
