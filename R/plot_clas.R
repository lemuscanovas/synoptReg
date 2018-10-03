#' Synoptic classification plot
#'
#' Plot the synoptic classification
#'
#' @param longitude Numeric. vector containing longitudes.
#' @param latitude Numeric. vector containing latitudes.
#' @param grouped_data Data frame. S-mode data frame containing an integer column with the weather types. i.e. output obtained from \code{synoptclas} function.
#' @param cwt_number Integer. Number of CWT to plot.
#' @param divide_units Integer to divide previous units. Default is 100.
#' @param zmin Integer. Minimum value to represent. Useful if you display many plots. Optional.
#' @param zmax Integer. Maximum value to represent. Useful if you display many plots. Optional.
#' @param legend.lab Character. Name of the variable used.
#' @param ... Other graphical parameters.
#'
#' @examples
#' # Load data (mslp)
#' data(mslp)
#' # Converting our data into a S-mode, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
#' # classification performance
#' smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
#' # Plot circulation weather type number 3
#' plot_clas(longitude = mslp$lon, latitude = mslp$lat,
#'           grouped_data = smode_clas$grouped_data,
#'           cwt_number = 3)
#'
#' @seealso  \code{\link{synoptclas}}
#'
#' @import fields maps ncdf4 raster zoo
#'
#' @export


plot_clas <- function(longitude, latitude, grouped_data, cwt_number,
                      divide_units = 100, zmin, zmax, legend.lab = "", ...) {
    CWT <- subset(grouped_data, CWT == cwt_number)
    CWT <- CWT[, -c(1:2)]
    CWT <- colMeans(CWT)/divide_units

    plot_cwt <- matrix(t(CWT), nrow = length(longitude),
                       ncol = length(latitude))
    rev_plot_cwt <- apply(plot_cwt, 1, rev)

    if (missing(zmin))
        zmin <- round(min(CWT) - 1)
    if (missing(zmax))
        zmax <- round(max(CWT) + 1)

    fields::image.plot(longitude, rev(latitude), t(rev_plot_cwt),
                       xlab = "Longitude", ylab = "Latitude",
                       zlim = c(zmin, zmax),
                       legend.args = list(text = legend.lab, side = 4,
                                          font = 1, line = 3, cex = 0.6),
                       ... = ...)

    graphics::contour(longitude, rev(latitude), t(rev_plot_cwt), xlab = "",
            ylab = "", lwd = 1.4,
        add = T, zlim = c(zmin, zmax), nlevels = (zmax - zmin)/2)  #col = ColorRamp(20), add = T)#,zlim=c(1004,1038))
    maps::map(add = TRUE, resolution = 0)
}
