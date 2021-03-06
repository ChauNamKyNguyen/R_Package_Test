% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize reading data.}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{An integer or string vector describes years from which coressponding files will be read.}
}
\value{
This function returns a data frame with summerize values for each year and month.
}
\description{
This function summarizes import data by years and months
You can customize years to read by using the \code{years}.
}
\examples{
\dontrun{
fars_summarize_years(c(2017:2018))
fars_summarize_years(c("2017","2018"))
}


}
