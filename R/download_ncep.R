#' Download NCEP/NCAR data
#'
#' Weather Data from NCEP/NCAR Reanalysis via RNCEP package
#'
#'
#' @param var slp 'sea level pressure' (default) for more variables see help of ?NCEP.gather
#' @param level surface (default)
#' @param month_range min,max month c(1,12) (default)
#' @param year_range min,max year c(2010,2017) (default)
#' @param lat_range min,max latitude c(30, 60) (default)
#' @param lon_range min,max longitud c(-30, 10) (default)
#' @param dailymean daily avarage of the variable retrived. Default TRUE.
#' @param hour One hour of the following: 0,6,12 or 18.
#' @param reanalysis2 Logical. Default TRUE. variables are downloaded from the NCEP-DOE Reanalysis 2. If FALSE, data downloaded from NCEP/NCAR Reanalysis 1
#' @param save_download Logical. Default TRUE. Do yoy want to save the downloaded data into an RData file?
#' @param file_name character. Provide a name for the file downloaded.
#'
#' @examples
#' \dontrun{
#' #Daily mean air temperature 2m for 2017
#' #ta_data <- download_ncep(year_range=2017)
#'
#' #Air temperature 2m at 06:00 for 2017
#' #ta_data_h6 <- download_ncep(year_range=2017,dailymean = FALSE,hour=6)
#'}
#'
#' @return a data.frame with the following variables: \code{lon, lat, time, value}
#'
#' @import RNCEP
#' @importFrom lubridate ymd_h
#'
#'
#' @export

download_ncep <- function(var = "slp", level = "surface",
                          month_range = c(1, 12), year_range = c(2010, 2017),
                          lat_range = c(30, 60), lon_range = c(-30, 10),
                          dailymean = TRUE, hour = NULL, reanalysis2 = TRUE,
                          save_download = TRUE, file_name = NULL) {


    # download with NCEP.gather function from RNCEP
    data_mat <- RNCEP::NCEP.gather(var, level, month_range, year_range, lat_range, lon_range, reanalysis2 = reanalysis2)

    if (dailymean == TRUE) {
        data_mat <- RNCEP::NCEP.aggregate(data_mat, HOURS = FALSE, fxn = "mean")
    } else {

        # Specific hour
        if (!is.null(hour)) {

            if (hour == 0) {
                data_mat <- RNCEP::NCEP.restrict(data_mat, hours2remove = c(6, 12, 18), set2na = FALSE)

            } else if (hour == 6) {
                data_mat <- RNCEP::NCEP.restrict(data_mat, hours2remove = c(0, 12, 18), set2na = FALSE)

            } else if (hour == 12) {
                data_mat <- RNCEP::NCEP.restrict(data_mat, hours2remove = c(0, 6, 18), set2na = FALSE)

            } else if (hour == 18) {
                data_mat <- RNCEP::NCEP.restrict(data_mat, hours2remove = c(0, 6, 12), set2na = FALSE)
            }
        }

    }
    # extract lonlat
    lat <- as.numeric(dimnames(data_mat)[[1]])
    lon <- as.numeric(dimnames(data_mat)[[2]])

    # expand lonlat for each point
    lonlat <- expand.grid(lon, lat)
    names(lonlat) <- c("lat", "lon")

    # extract date-time
    time <- dimnames(data_mat)[[3]]

    if (dailymean == TRUE) {

        time <- lubridate::ymd(time)

    } else {

        time <- lubridate::ymd_h(time)

    }

    dimnames(data_mat) <- NULL

    data_mat <- aperm(data_mat, c(2, 1, 3))
    df <- data.frame(lonlat,
                     t(data.frame(matrix(data_mat, nrow = dim(data_mat)[3],
                                         byrow = TRUE)))) %>%
        setNames(c("lon", "lat", as.character(time))) %>%
        tibble::as_tibble() %>%
        tidyr::gather(key = time, value = "value", -1:-2) %>%
        dplyr::mutate(time = as_date(time))


    # #in case you want to save the downloaded matrix
    if (save_download == TRUE) {
        save(df, file = paste(var, "_grid.RData", sep = ""))
    }
    return(df)

}
