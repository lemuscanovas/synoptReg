#' Read and tidy your NetCDF
#'
#' Read and tidy your initial NetCDF by turning the absolute values into monthly-based anomalies, and by subsetting the time series and geogprahical area of your atmospheric variable.

#' @param x path to file or SpatRaster object.
#' @param anomaly logical. If TRUE it convets into anomalies, based on their corresponding monthly means.
#' @param time_subset character or Date. Default NULL. Provide a vector of dates to subset the original SpatRaster time series.
#' @param month_subset integer. Default NULL. Provide a vector of integers to subset the original SpatRaster by months.
#' @param crop_area integer. Default NULL. Provide a vector of coordinates (xmin,xmax,ymin,ymax) to crop the original SpatRaster domain.
#' @param aggregate integer. Default NULL. Resampling to a coarser resolution. Useful to save memory when processing heavy time-consuming datasets.
#' 
#' @return A SpatRaster object. It must be converted to daily if the input is hourly.
#'
#' @examples
#' # Load data (mslp or precp_grid)
#' slp_file <- system.file("extdata", "mslp_ei.nc", package = "synoptReg")
#' 
#' # Reading data simply
#' slp <- read_nc(slp_file)
#'
#' # Converting to monthly based anomalies and just for October, November and December
#' slp <- read_nc(slp_file, anomaly = TRUE, month_subset = 10:12)
#'
#' @seealso
#' \code{\link{read_nc}}
#'
#' @importFrom lubridate as_date is.POSIXct month
#' @importFrom methods is
#' @importFrom stats prcomp var
#' @importFrom terra app crop nlyr
#' @export

read_nc <- function(x, anomaly = F, time_subset = NULL, month_subset = NULL, 
                    crop_area = NULL, aggregate = NULL){
  
  if(is(x, "SpatRaster")){
    dat <- x
  } else {
    dat <- rast(x)
  }
  varname <- varnames(dat) %>% unique
  unit <- units(dat) %>% unique
  dates <- terra::time(dat)
  if(sum(inherits(dates, "Date") + inherits(dates, "POSIXct") == 0)){
    stop("Not readable time string or not provided!")
  }
  
  dates_daily <- as_date(dates)
  
  if(any(dates_daily == lag(dates_daily),na.rm = T) == T){
  dat <- tapp(dat,as.factor(dates_daily),"mean")
  }
  terra::time(dat) <- unique(dates_daily)
  
  
  if(isTRUE(anomaly)){
    
    mean <- app(dat, "mean")
    dat <- dat - mean
  }
  
  
  varnames(dat) <- varname
  units(dat) <- unit
  
  
  
  if(!is.null(time_subset)){
    
    dates <- terra::time(var)
    class(dates)
    
    if(is(dates, "Date")){
      
      sel <- which(dates %in% as_date(time_subset))
      dat <- dat[[sel]]}
    
    else{
      
      stop("Data provided must be daily-based. Try use daily_mean = T")
      
    }
  }
  
  if(!is.null(month_subset)){
    dates <- terra::time(dat)
    class(dates)
    
    if(is(dates, "Date")){
      sel <- which(month(dates) %in% month_subset)
      dat <- dat[[sel]]
      
    }else{
      
      stop("Data provided must be daily-based. Try use daily_mean = T")
      
    }
  }
  
  if(!is.null(crop_area)){
    dat <- crop(dat, crop_area)
  }
  
  if(!is.null(aggregate)){
    dat <- terra::aggregate(dat, 2)
  }
  
  return(dat)
}
