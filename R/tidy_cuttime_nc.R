#' Format a 3D-array to a S-mode data frame and set the time period
#'
#' This function format the 3D-array output from \code{read_nc} function to a S-mode dataframe (variables = grid points, observations = days). Optionally, you can set the time period between specific years and/or specify if you want work with the full year or only with 3-month season.
#'
#' @param datalist List. 3D-array of atmospheric or environmental variable, longitudes, latitudes and time. I.e. the list returned by \code{read_nc}.
#' @param season Character. Name of the season wanted (i.g. "winter", "spring", "summer", "fall").
#' @param year_range vector. Starting and ending year wanted to subset the data.
#'
#' @return A list with: \itemize{
#'    \item{A vector of dates.}
#'    \item{A dataframe of the variable in S-mode.}
#'    \item{A character with the name of the season}
#' }
#'
#' @examples
#' # Load data (mslp or precp_grid)
#' data(mslp)
#' # Converting our data into a S-mode, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp)
#' # matrix conversion and setting time period for winters between 2001 and 2010
#' smode_mslp <- tidy_cuttime_nc(mslp, season = "winter",
#'                              year_range = c(2001,2010))
#'
#' @seealso
#' \code{\link{read_nc}}
#'
#' @export


tidy_cuttime_nc <- function(datalist, season = NULL, year_range = NULL) {

    full_dates <- as.Date(datalist$dates)
    year <- format(full_dates, "%Y")

    # generating a vector of seasons
    yq <- zoo::as.yearqtr(zoo::as.yearmon(full_dates, "%Y-%m-%Yd") + 1/12)
    yq2 <- factor(format(yq, "%q"), levels = 1:4, labels = c("winter", "spring",
                                                             "summer", "fall"))

    # array to matrix and then to dataframe in order to run PCA after
    mat_ncdata <- as.data.frame(t(matrix(datalist$datavar, dim(datalist$datavar)[1] *
        dim(datalist$datavar)[2])))
    mat_ncdata$season <- yq2
    mat_ncdata$year <- year
    rownames(mat_ncdata) <- full_dates

    # If year_range and season are not previously defined
    if (is.null(year_range) & is.null(season)) {
        cut_ncdata <- mat_ncdata
        cut_ncdata$year <- NULL
        cut_ncdata$season <- NULL

        return(list(dates = full_dates, smode_data = cut_ncdata))

    #If only season is changed (winter, spring, summer, autumn)
    } else if (is.null(year_range) & !is.null(season)) {
        
      if (season == "winter") {
        
        # cuting dates and data by winter seasony
        cut_ncdata <- subset(mat_ncdata, yq2 == "winter")
        cut_ncdata$season <- NULL
        cut_ncdata$year <- NULL
        
        cut_dates <- subset(full_dates, yq2 == "winter")
        rownames(cut_ncdata) <- cut_dates
        
      } else if (season == "spring") {
        
        # cuting dates and data by spring season
        cut_ncdata <- subset(mat_ncdata, yq2 == "spring")
        cut_ncdata$season <- NULL
        cut_ncdata$year <- NULL
        
        cut_dates <- subset(full_dates, yq2 == "spring")
        rownames(cut_ncdata) <- cut_dates
        
      } else if (season == "summer") {
        
        # cuting dates and data by summer season
        cut_ncdata <- subset(mat_ncdata, yq2 == "summer")
        cut_ncdata$season <- NULL
        cut_ncdata$year <- NULL
        
        cut_dates <- subset(full_dates, yq2 == "summer")
        rownames(cut_ncdata) <- cut_dates
        
      } else if (season == "fall") {
        
        # cuting dates and data by fall season
        cut_ncdata <- subset(mat_ncdata, yq2 == "fall")
        cut_ncdata$season <- NULL
        cut_ncdata$year <- NULL
        
        cut_dates <- subset(full_dates, yq2 == "fall")
        rownames(cut_ncdata) <- cut_dates
        
      }     
    
      
    # If only year_range is defined by the user 
    } else if (!is.null(year_range) & is.null(season)) {
  
        cut_ncdata <- subset(mat_ncdata, year >= year_range[1] &
                           year <= year_range[2])
        cut_ncdata$year <- NULL
        cut_ncdata$season <- NULL
    
        cut_dates <- subset(full_dates, year >= year_range[1] &
                          year <= year_range[2])
        rownames(cut_ncdata) <- cut_dates
        
    
    # If year_range and season are defined by the user 
    } else if (!is.null(year_range) & !is.null(season)) {

        # cuting dates and data by year
        if (season == "year") {

            cut_ncdata <- subset(mat_ncdata, year >= year_range[1] &
                                   year <= year_range[2])
            cut_ncdata$year <- NULL
            cut_ncdata$season <- NULL

            cut_dates <- subset(full_dates, year >= year_range[1] &
                                  year <= year_range[2])
            rownames(cut_ncdata) <- cut_dates

        } else if (season == "winter") {

            # cuting dates and data by winter seasony
            cut_ncdata <- subset(mat_ncdata, year >= year_range[1] &
                                   year <= year_range[2] & yq2 == "winter")
            cut_ncdata$season <- NULL
            cut_ncdata$year <- NULL

            cut_dates <- subset(full_dates, year >= year_range[1] &
                                  year <= year_range[2] & yq2 == "winter")
            rownames(cut_ncdata) <- cut_dates

        } else if (season == "spring") {

            # cuting dates and data by spring season
            cut_ncdata <- subset(mat_ncdata, year >= year_range[1] &
                                   year <= year_range[2] & yq2 == "spring")
            cut_ncdata$season <- NULL
            cut_ncdata$year <- NULL

            cut_dates <- subset(full_dates, year >= year_range[1] &
                                  year <= year_range[2] & yq2 == "spring")
            rownames(cut_ncdata) <- cut_dates

        } else if (season == "summer") {

            # cuting dates and data by summer season
            cut_ncdata <- subset(mat_ncdata, year >= year_range[1] &
                                   year <= year_range[2] & yq2 == "summer")
            cut_ncdata$season <- NULL
            cut_ncdata$year <- NULL

            cut_dates <- subset(full_dates, year >= year_range[1] &
                                  year <= year_range[2] & yq2 == "summer")
            rownames(cut_ncdata) <- cut_dates

        } else if (season == "fall") {

            # cuting dates and data by fall season
            cut_ncdata <- subset(mat_ncdata, year >= year_range[1] &
                                   year <= year_range[2] & yq2 == "fall")
            cut_ncdata$season <- NULL
            cut_ncdata$year <- NULL

            cut_dates <- subset(full_dates, year >= year_range[1] &
                                  year <= year_range[2] & yq2 == "fall")
            rownames(cut_ncdata) <- cut_dates

        } else {
            stop("Wrong season specified. Data and dates not fit correctly!")
        }
    }
    return(list(dates = cut_dates, smode_data = cut_ncdata, season = season))
}
