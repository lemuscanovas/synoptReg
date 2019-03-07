#' Download NCEP
#'
#' Weather Data from NCEP/NCAR Reanalysis via RNCEP package
#' 
#' @import RNCEP
#' 
#' @param var slp 'sea level pressure' (default) for more variables see help of ?NCEP.gather
#' @param level surface (default)
#' @param month_range min,max month c(1,12) (default)
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
                           dailymean=TRUE,
                           reanalysis2=TRUE,
                           save_download=TRUE,
                           file_name=NULL){

  #download with NCEP.gather function from RNCEP
  data_mat <-        NCEP.gather(var,level,
                                 month_range,
                                 year_range,
                                  lat_range,
                                  lon_range,
                         reanalysis2 = reanalysis2)
  
  #extract lonlat
  lat <- as.numeric(dimnames(data_mat)[[1]])
  lon <- as.numeric(dimnames(data_mat)[[2]])
  #expand lonlat for each point
  lonlat <- expand.grid(lon,lat)
  names(lonlat) <- c("lat","lon")
  
  #extract date-time
  time <- dimnames(data_mat)[[3]]
  time <- lubridate::ymd_h(time)
  
  dimnames(data_mat) <- NULL
  
  ncep_list <- list(mat=data_mat,time=time,coord=lonlat)
 
  #in case you want to save the downloaded matrix
  if(save_download==TRUE) save(ncep_list,file=paste(var,"_grid.RData",sep="")) 


  return(ncep_list)
  
}


