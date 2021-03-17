#' Set the time period and the geogprahical extension, as well as computes the anomaly of the atmospheric variable/s
#'
#' This function allows to subset the time series and geogprahical area of your atmospheric variable.
#' In addition, even if no argument is given, the anomaly of the atmospheric variable/s will be computed.
#' The anomaly value is provided in order to facilitate the visualization of the results after use the \code{synoptclas} function.
#' It is mandatory to pass the \code{tidy_nc} even if you do not want to change the time period or the geographical extension.
#'
#' @param x data.frame. A data.frame with the following variables: \code{lon, lat, time, value}. The same structure returned when using \code{download_ncep}.
#' @param time_subset vector. Starting and ending date, or a vector of dates of interest.
#' @param geo_subset vector. A vector providing the \code{xmin,xmax,ymin,ymax}.
#' @param monthly_subset an integer or a vector of integers. Number of the month/s desired.
#' @param name_vars character or a vector of characters. Name of the atmospheric variable/s. If name is not specified, then will be coded as integers.
#'
#' @return A data.frame with the following variables: \code{lon, lat, time, value, anom_value}
#'
#' @examples
#' # Load data (mslp or precp_grid)
#' data(mslp)
#' data(z500)
#' # Tidying our atmospheric variables (500 hPa geopotential height
#' # and mean sea level pressure) together.
#'
#' # Time subset between two dates
#' atm_data1 <- tidy_nc(x = list(mslp,z500), time_subset = c("2000-05-01","2001-04-30"))
#'
#' # Time subset using a vector of dates of interest. Including a geographical crop
#' dates_int <- c("2000-01-25","2000-04-01","2000-07-14","2001-05-08","2002-12-20")
#' atm_data1 <- tidy_nc(x = list(mslp,z500),
#'                      time_subset = dates_int,
#'                      geo_subset = c(-20,10,30,50),
#'                      name_vars = c("mslp","z500")) # following the list sequence
#'
#' @seealso
#' \code{\link{download_ncep}}
#'
#' @import dplyr
#' @importFrom lubridate as_date year month day
#'
#' @export


# x pot ser un df o llista

tidy_nc <- function(x, time_subset = NULL, geo_subset = NULL,
                    monthly_subset = NULL, name_vars = NULL) {

    x <- bind_rows(x, .id = "var")

    if (is.null(time_subset)) {
        x <- x %>%
          mutate(mo = month(time)) %>%
          group_by(.data$lon, .data$lat, .data$var, .data$mo) %>%
          mutate(mean_mo = mean(.data$value)) %>%
          ungroup() %>%
          mutate(anom_value = .data$value - .data$mean_mo) %>%
          select(-.data$mo, -.data$mean_mo)
    } else {

    }
    if (!is.null(name_vars)) {
        var_names <- rep(name_vars, each = sum(x$var == 1))
        x <- mutate(x, var = var_names)
    }

    if (!is.null(time_subset) & length(time_subset) < 3) {
        x <- filter(x, time >= as_date(time_subset[1]),
                    time <= as_date(time_subset[2])) %>%
          mutate(mo = month(.data$time)) %>%
          group_by(.data$lon, .data$lat, .data$var, .data$mo) %>%
          mutate(mean_mo = mean(.data$value)) %>%
          ungroup() %>%
          mutate(anom_value = .data$value - .data$mean_mo) %>%
          select(-.data$mo, -.data$mean_mo)  # anom

    } else if (!is.null(time_subset) & length(time_subset) > 2) {
        x <- x %>%
          filter(.data$time >= as_date(time_subset[1]),
                 .data$time <= as_date(time_subset)[order(as_date(time_subset))][length(time_subset)]) %>%
          mutate(mo = month(.data$time)) %>%
          group_by(.data$lon, .data$lat, .data$var, .data$mo) %>%
          mutate(mean_mo = mean(.data$value)) %>%
          ungroup() %>%
          mutate(anom_value = .data$value - .data$mean_mo) %>%
          select(-.data$mo, -.data$mean_mo)  # anom

        # filtering specific dates
        x <- filter(x, .data$time %in% as_date(time_subset))
    }

    if (!is.null(geo_subset)) {
        x <- filter(x, .data$lon >= geo_subset[1],
                    .data$lon <= geo_subset[2] | .data$lat >= geo_subset[3],
                    .data$lat <= geo_subset[4])
    }
    if (!is.null(monthly_subset)) {
        x <- mutate(x, month = month(.data$time)) %>%
          filter(month %in% monthly_subset)

    }
    return(x)
}
