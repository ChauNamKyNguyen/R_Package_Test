#' Import data from an external filename
#'
#' This function imports data from an external CSV file.
#' You can customize which file to be read by using the argument \code{filename}.
#'
#' @param filename A character string giving the location to import data.
#'    The value of this argument should describe the full path of an external file.
#'
#' @return This function returns a data frame tbl.
#'
#' @note This function will return an error when a file doesn't exist.
#'
#' @import dplyr readr
#'
#' @examples
#' \dontrun{
#' fars_read("my_input.csv")
#' }
#'
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create a file name from a given year
#'
#' This function create a file name from a given year.
#' You can customize a year to create by using the argument \code{year}.
#'
#' @param year An integer or string scale giving a year from which the function will create a file name.
#'
#' @return This function returns a full path for a raw data file.
#'
#' @examples
#' make_filename(2018)
#' make_filename("2018")
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)

  # UPDATED: https://www.coursera.org/learn/r-packages/discussions/weeks/4/threads/dL-ab2OKEeeT6Aqv2QJASg
  file <- sprintf("accident_%d.csv.bz2", year)
  system.file("extdata", file, package = "fars")
  # END UPDATED
}

#' Create a list for a vector of years.
#'
#' This function reads each file from given years.
#' You can customize years to read by using the argument \code{years}.
#'
#' @param years An integer or string vector that describes years from which a coressponding files will be read.
#'
#' @return This function returns a list of month and year for reading data.
#'
#' @import dplyr
#'
#' @note This function will return an error if file doesn't exist with an error and warning message.
#' The error tells which file doesn't exist, while the warning indicates which year of the vector is invalid.
#'
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013:2015))
#' fars_read_years(c("2013","2015"))
#' }
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      mutate(dat, year = year) %>%
        select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize reading data.
#'
#' This function summarizes import data by years and months
#' You can customize years to read by using the \code{years}.
#'
#' @param years An integer or string vector describes years from which coressponding files will be read.
#'
#' @return This function returns a data frame with summerize values for each year and month.
#'
#' @import dplyr tidyr
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2017:2018))
#' fars_summarize_years(c("2017","2018"))
#' }
#'
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Create a plot from a given state and year.
#'
#' This is a simple function that plot a graphic for a state in a year.
#' You can customize a state and year to read (using the \code{state.num} and the \code{year} argument)
#' For NA values of longtitude, the function will replace with logicall values of comparison between
#' longtiude and 900.
#' For NA values of latitude, the function will replace with logicall values of comparison between
#' latitude and 90.
#'
#' @param state.num An integer or string scala of a state code
#'
#' @param year An integer or string scala of a year
#'
#' @return This NULL for invalid state or year or accident to plot.
#'
#' @note If a state is invalid, an error will display. If there is no accident, a info message will display.
#'
#' @import maps graphics
#'
#' @examples
#' \dontrun{
#' fars_map_state(4, 2013)
#' fars_map_state("4","2013")
#' }
#'
#' @export


fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
