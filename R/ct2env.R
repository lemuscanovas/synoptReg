#' Establishing the relationship between CT and a environmental variable
#'
#' This function applies the approach: "circulation types to environment".
#'
#' @param x data.frame. A data.frame containing the environmental data (i.e. precipitation, temperature, PM10, etc.) with the following variables: \code{lon, lat, time, value, anom_value}. See \code{tidy_nc}.
#' @param clas data.frame. A data.frame of the synoptic classification (time and WT) obtained from the \code{synoptclas} function.
#' @param fun function. A function to be applied to the environmental variable for each WT.
#' @param out character. Choose between \code{"data.frame"} (default) or \code{"raster"} A function to be applied to the environmental variable for each WT.
#'
#' @return a data.frame or a Raster Stack containing the environmental grids based on the weather types.
#'
#' @examples
#' # Load data (mslp or precp_grid)
#' data(mslp)
#' data(z500)
#' # Tidying our atmospheric variables (500 hPa geopotential height
#' # and mean sea level pressure) together.
#'
#' # Time subset between two dates
#' atm_data1 <- tidy_nc(x = list(mslp,z500),
#'              name_vars = c("mslp","z500"))
#'
#' # S-mode classification
#' smode_clas <- synoptclas(atm_data1, ncomp = 6)
#'
#' # ct2env (precipitation example)
#' ct2env(x = pcp, clas = smode_clas$clas, fun = mean, out = "data.frame")
#'
#'
#' @export

ct2env <- function(x, clas, fun = mean, out = "data.frame") {

    FUN <- match.fun(fun)

    x <- x %>%
        inner_join(clas, by = "time") %>%
        group_by(.data$WT, .data$lon, .data$lat) %>%
        mutate(calc = FUN(.data$value)) %>%
        ungroup() %>%
        distinct(.data$WT, .data$lon, .data$lat, .keep_all = T) %>%
        select(-.data$value, -.data$time)

    if (out == "data.frame") {

        return(x)

    } else if (out == "raster") {

        x <- x %>%
            spread(.data$WT, .data$calc, drop = T) %>%
            distinct(.data$lon, .data$lat, .keep_all = T) %>%
            filter_at(3, all_vars(!is.na(.data$.))) %>%
            raster::rasterFromXYZ()
        return(x)
    }
}
