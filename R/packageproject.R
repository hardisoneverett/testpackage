
#' fars_read
#'
#' This function reads in data from a file.  If the file named does not exist,
#' an error message is shown.
#'
#' @param filename A character vector.
#' @return If the file named exists, the function returns a dataframe. If the
#' file named does not exist, then an error message is returned.
#'
#' @examples \dontrun{fars_read("filename")}
#' @export
fars_read <- function(filename) {
        require(dplyr)
        require(readr)

        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename
#'
#' This function creates a name for a file given a year.
#'
#' @param year An integer vector.
#'
#' @return A character vector to be used as a file name.
#'
#' @examples \dontrun{make_filename(2009)}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This function takes in an integer vector and using the \code{\link{make_filename}}
#' function creates file names.  Then using the \code{\link{fars_read}} function
#' it searches if those files exist. If the files exist and are the correct
#' format then a dataframe is returned, otherwise a warning is returned. The
#' returned dataframe has two variables (Month and year).  If multiple years
#' are provided, multiple dataframes are returned.
#'
#' @param years Integer(s).
#'
#' @return A dataframe with two variables (Month and year) or an error message.
#'
#' @examples \dontrun{fars_read_years(2013)}
#' @examples \dontrun{fars_read_years(2013:2015)}
#'
#' @export
fars_read_years <- function(years) {
        require(dplyr)
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' fars_summarize_years
#'
#' This function takes in a list of years and outputs a summarized table of
#' fatalities by month by year.  If the data files do not exist or are in the
#' incorrect format an error message will appear.
#'
#' @param years Integer vector
#'
#' @return A table of fatalities summarized by month by year.
#'
#' @examples \dontrun{fars_summarize_years(2013)}
#'
#' @export
fars_summarize_years <- function(years) {
        require(dplyr)
        require(tidyr)
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)


}

#' fars_map_state
#'
#' This function takes in a number representing a state and a year.  The
#' function outputs a map with the state's lines and accident locations.
#' If the \code{state.num} is not in the data then an error is returned. If
#' the file cannot be found or is in the incorrect format and error will also
#' be shown.
#'
#' @param state.num A Integer
#' @param year Integer
#'
#' @return A map with state lines and accident locations (mapped as points).
#' If there is no data to plot, then that message will be shown.
#'
#' @examples \dontrun{fars_map_state(9,2013)}
#'
#' @export
fars_map_state <- function(state.num, year) {
        require(maps)
        require(dplyr)
        require(graphics)
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


