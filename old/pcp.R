#' Daily precipitation grid of Balearic Islands (Spain)
#'
#' Data from the SPREAD data set downloaded from the Spanish National Research Council (CSIC).
#' (\url{http://spread.csic.es/info.html}).
#' This data corresponds to daily values of precipitation with a spatial resolution of 5 x 5 km from January 2000 to December 2010
#'
#' @docType data
#'
#' @usage pcp_spread
#'
#' @format SpatRaster time series of daily precipitation.
#' \describe{
#'   \item{geographical area:}{Balearic Islands}
#'   \item{time period:}{2000-01-01 to 2010-12-31}
#'   \item{units:}{mm}
#'   \item{coordinates reference system:}{+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs}
#' }
#' @keywords datasets
#'
#' @references Serrano-Notivoli et al. (2017)
#' \emph{SPREAD: a high-resolution daily gridded precipitation dataset for Spain, an extreme events frequency and intensity overview.
#'              Earth Syst. Sci. Data, 9, 721-738, 2017, https://doi.org/10.5194/essd-9-721-2017}
#'
#' @examples
#' rast(pcp_spread)
"pcp_spread"
