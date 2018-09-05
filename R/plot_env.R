#' Environmental data plot based on CWT
#'
#' Plot the daily mean spatial distribution of an environmental data based on the synoptic classification
#'
#' @param longitude Numeric. vector containing longitudes.
#' @param latitude Numeric. vector containing latitudes.
#' @param cluster_data Integer containing the results of the synoptic classification.
#' @param grid_data Data frame containing the environmental data (i.e. precipitation, temperature, PM10, etc.)
#' @param cwt_number Integer. Number of CWT to plot.
#' @param option Integer (1 or 2), to manage latitude and longitude data when plot. Try 2 if 1 is wrong and viceversa. Default is 1.
#' @param divide_units Integer to divide previous units. Default is 1.
#' @param na.rm Logical. If TRUE, all the grid points are used to calculate the daily mean although NA exists. If FALSE, only grid points with the complete serie are used to compute the daily mean. Default is TRUE.
#' @param zmin Integer. Minimum value to represent. Useful if you display many plots. Optional.
#' @param zmax Integer. Maximum value to represent. Useful if you display many plots. Optional.
#' @param legend.lab Character. name of the variable used.
#' @param ... Other graphical parameters.
#'
#' @examples
#' # Load data (precp_grid and mslp)
#' data(precp_grid)
#' data(mslp)
#' # Converting our data, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
#' precp_data <- tidy_cuttime_nc(precp_grid, only_convert = TRUE)
#' # classification performance
#' smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
#' # Plot precipitation data based on cwt 3
#' plot_env(longitude = precp_grid$lon, latitude = precp_grid$lat,
#'          cluster_data = smode_clas$clas, grid_data = precp_data$smode_data,
#'          cwt_number = 3, option = 2, divide_units = 10, legend.lab = "mm")
#'
#' @seealso  \code{\link{synoptclas}}
#'
#' @export

plot_env <- function(longitude, latitude, cluster_data, grid_data,
                      cwt_number = 1, option = 1, na.rm = T, zmin,
                      zmax, divide_units = 1, legend.lab = "", ...) {

    if (option == 1) {
        grid_cwt <- cbind.data.frame(cluster_data, grid_data)
        colnames(grid_cwt)[1] <- "cwt"
        pcp_by_cwt <- subset(grid_cwt, grid_cwt$cwt == cwt_number)
        pcp_by_cwt <- pcp_by_cwt[, -1]
        mean_pcp_by_cwt <- colMeans(pcp_by_cwt, na.rm = na.rm)/divide_units

        plot_precp <- matrix(t(mean_pcp_by_cwt), nrow = length(longitude), ncol = length(latitude))

        if (missing(zmax))
            zmax <- round(max(mean_pcp_by_cwt, na.rm = T) + 1)
        if (missing(zmin))
            zmin <- 0

        fields::image.plot(longitude, latitude, plot_precp, xlab = "Longitude", ylab = "Latitude",
            zlim = c(zmin, zmax), legend.args = list(text = legend.lab, side = 4, font = 1,
                line = 3, cex = 0.6), ... = ...)
        maps::map(add = TRUE, resolution = 0)

    } else if (option == 2) {
        grid_cwt <- cbind.data.frame(cluster_data, grid_data)
        colnames(grid_cwt)[1] <- "cwt"
        pcp_by_cwt <- subset(grid_cwt, grid_cwt$cwt == cwt_number)
        pcp_by_cwt <- pcp_by_cwt[, -1]
        mean_pcp_by_cwt <- colMeans(pcp_by_cwt)/divide_units

        plot_precp <- matrix(t(mean_pcp_by_cwt), nrow = length(latitude), ncol = length(longitude))

        if (missing(zmax))
            zmax <- round(max(mean_pcp_by_cwt, na.rm = T) + 1)
        if (missing(zmin))
            zmin <- 0

        fields::image.plot(longitude, latitude, t(plot_precp),
                           xlab = "Longitude", ylab = "Latitude",
                           zlim = c(0, zmax),
                           legend.args = list(text = legend.lab,
                                              side = 4, font = 1,
                                              line = 3, cex = 0.6),
                           ... = ...)
        maps::map(add = TRUE, resolution = 0)

    } else {
        stop("check latitudes and longitudes!")
    }
}
