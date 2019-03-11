#' Download NCEP
#'
#' Weather Data from NCEP/NCAR Reanalysis via RNCEP package
#'
#' @import RNCEP
#'
#' @param var slp 'sea level pressure' (default) for more variables see help of ?NCEP.gather
#' @param level surface (default)
#' @param month_range min,max month c(1,12) (default)
#' @param hour One or several hours of the following: 0,6,12 or 18 c(0,6) or 1
#'
#' @example
#' #Air temperature 2m for 2017
#' ta_data <- download_ncep(year_range=2017)
#'
#'
#' @return a list with: itemize{
#'    \item{a matrix with the reanalysis data}
#'    \item{a date-time object}
#'    \item{a data.frame with longitude and latitude for each point}
#' }
#'
#' @export


download_ncep <- function(var="air.2m",level="gaussian",
                           month_range=c(1,12),
                           year_range=c(2010,2017),
                           lat_range=c(30,60),
                           lon_range=c(-30,10),
                           dailymean=FALSE,
                           hour = NULL,
                           reanalysis2=TRUE,
                           save_download=TRUE,
                           file_name=NULL){

  #argument control

  if (!(hour%in%c(0,6,12,18))) stop("'hour' must be one of the following: 0,6,12,18")


  #download with NCEP.gather function from RNCEP
  data_mat <-        NCEP.gather(var,level,
                                 month_range,
                                 year_range,
                                 lat_range,
                                 lon_range,
                         reanalysis2 = reanalysis2)

  if(dailymean == TRUE){
    data_mat <- NCEP.aggregate(data_mat, HOURS=FALSE, fxn='mean')
  }

  # Specific hour
 if(!is.null(hour)){

    if(hour == 0){
      data_mat <-        NCEP.restrict(data_mat,hours2remove = c(6,12,18),set2na = FALSE )

    }else if(hour == 6) {
     data_mat <-        NCEP.restrict(data_mat,hours2remove = c(0,12,18), set2na = FALSE)

    }else if(hour == 12) {
      data_mat <-        NCEP.restrict(data_mat,hours2remove = c(0,6,18),set2na = FALSE )

    }else if(hour == 18) {
      data_mat <-        NCEP.restrict(data_mat,hours2remove = c(0,06,12),set2na = FALSE )
    }
  }


  #extract lonlat
  lat <- as.numeric(dimnames(data_mat)[[1]])
  lon <- as.numeric(dimnames(data_mat)[[2]])
  #expand lonlat for each point
  # lonlat <- expand.grid(lon,lat)
  # names(lonlat) <- c("lat","lon")

  #extract date-time
  time <- dimnames(data_mat)[[3]]
  time <- lubridate::ymd_h(time)

  dimnames(data_mat) <- NULL

  ncep_list <- list(datavar=data_mat, lon=lon, lat=lat, dates=time)

  #in case you want to save the downloaded matrix
  if(save_download==TRUE){
    save(ncep_list,file=paste(var,"_grid.RData",sep=""))
  }

  return(ncep_list)

}
