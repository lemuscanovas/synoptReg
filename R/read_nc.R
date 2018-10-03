#' Read a NetCDF file
#'
#' This function read a NetCDF file through \pkg{ncdf4} package, to extract the atmospheric or environmental variable, longitudes, latitudes and dates. A continuous NetCDF withouth date gaps is required.
#'
#' @param nc_input NetCDF path with atmospheric or environmental field (mean sea level pressure, geopotential height, precipitation, ...).
#' @param name_coord Character. Names of longitude, latitude and time coordinates.
#' @param initial_date Character. Start date of the NetCDF. As character format.
#'
#' @return a list with: \itemize{
#'    \item{A 3D-array (lon, lat, times) of atmospheric variable.}
#'    \item{A numeric with longitude values.}
#'    \item{A numeric with latitude values.}
#'    \item{A Date format vector containing dates.}
#' }
#'
#' @export

read_nc <- function(nc_input, name_coord, initial_date) {

    ncdata <- ncdf4::nc_open(nc_input)
    # Extract Coordinates
    lon_name <- name_coord[1]
    lat_name <- name_coord[2]
    time_name <- name_coord[3]

    lat_vals <- ncdata$dim[[lat_name]]$vals
    lon_vals <- ncdata$dim[[lon_name]]$vals
    time_vals <- ncdata$dim[[time_name]]$vals

    dates <- as.Date(initial_date) + 0:length(time_vals[-1])

    namevar <- ncdata$var[[1]]$name

    # To get the data
    atmosdata <- ncdf4::ncvar_get(ncdata, namevar, start = c(1, 1, 1),
                                  count = c(-1, -1, -1))


    ncdf4::nc_close(ncdata)
    return(list(datavar = atmosdata, lon = lon_vals, lat = lat_vals,
                dates = dates))
}
