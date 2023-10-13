#' Mean Sea Level pressure data
#'
#' Data from the NCEP/NCAR Reanalysis 1
#' (\url{ https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.html}).
#' This data corresponds to daily values of mean sea level pressure with 2.5 x 2.5º of spatial resolution from January 2000 to december 2002.
#'
#' @docType data
#'
#' @usage data(msl)
#'
#' @format A data.frame with the following variables: \code{x,y,time,value,var,units}.
#' \describe{
#'   \item{geographical area:}{-10,30,30,60}
#'   \item{time period:}{2000-01-01 to 2002-12-31}
#'   \item{units:}{Pascals}
#' }
#' @keywords datasets
#'
#' @references Kalnay et al. (1996)
#' \emph{The NCEP/NCAR 40-year reanalysis project,
#'                    Bull. Amer. Meteor. Soc., 77, 437-470, 1996}
#'
#' @examples
#' data(msl)
"msl"
