% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Create a plot from a given state and year.}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer or string scala of a state code}

\item{year}{An integer or string scala of a year}
}
\value{
This NULL for invalid state or year or accident to plot.
}
\description{
This is a simple function that plot a graphic for a state in a year.
You can customize a state and year to read (using the \code{state.num} and the \code{year} argument)
For NA values of longtitude, the function will replace with logicall values of comparison between
longtiude and 900.
For NA values of latitude, the function will replace with logicall values of comparison between
latitude and 90.
}
\note{
If a state is invalid, an error will display. If there is no accident, a info message will display.
}
\examples{
\dontrun{
fars_map_state(30, 2018)
fars_map_state("30","2018")
}

}
