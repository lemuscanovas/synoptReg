#' Convert a SpatRaster object to a data.frame structure required by synoptReg
#'
#' Convert a SpatRaster from the terra package to a data.frame formatted in the way
#' required by synoptReg .
#'
#' @param x SpatRaster time series containing the atmospheric data (e.g. mean sea level pressure).
#'
#' @return A data.frame with the following variables: \code{x, y, time, value, var, units}
#'
#' @examples
#' # Load data
#' slp_file <- system.file("extdata", "mslp_ei.nc", package = "synoptReg")
#' 
#' # Reading data
#' slp <- read_nc(slp_file)
#'
#' # Converting it to the required structure by synoptReg
#' slp_df <- as_synoptReg(slp)
#'
#' @seealso
#' \code{\link{read_nc}}
#'
#' @importFrom terra as.data.frame varnames units rast varnames<- units<-
#' @importFrom lubridate as_date is.Date
#' @importFrom dplyr mutate select filter
#' @importFrom stringr str_remove str_replace
#' 
#'
#' @export

as_synoptReg <- function(x){
  
  varname <- unique(varnames(x))
  unit <- unique(units(x))
  times <- time(x)
  
  df <- terra::as.data.frame(x, xy = TRUE) %>%
    setNames(c("x","y",paste0("X",times))) %>%
    pivot_longer(names_to = "time",values_to = "value", 3:ncol(.))
  
  if(str_length(as_date(times[1])) == 10){
    df <- mutate(df, time = as_date(str_remove(time, "X")),
           var = varname,
           units = unit)
  } else {
    df <- mutate(df, time = as_datetime(str_remove(time, "X")),
                 var = varname,
                 units = unit)
  } 
  return(df)
}
