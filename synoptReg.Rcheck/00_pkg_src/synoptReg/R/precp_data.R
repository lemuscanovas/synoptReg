#' Daily precipitation grid of Balearic Islands (Spain)
#'
#' Data from a SPREAD data set downloaded from CSIC.
#' (\url{http://digital.csic.es/handle/10261/141218}).
#' This data corresponds to daily values of precipitation with 5 x 5 km resolution from January 2000 to december 2009.
#'
#' @docType data
#'
#' @usage data(precp_grid)
#'
#' @format A list with values of pressure and coordinates (longitude, latitude, time)
#' \describe{
#'   \item{datavar}{daily precipitation values, "mm*10"}
#'   \item{lon}{53}
#'   \item{latitude}{35}
#'   \item{dates}{3653, ten years (2000-01-01 / 2009-12-31)}
#' }
#' @keywords datasets
#'
#' @references Serrano-Notivoli et al. (2017)
#' \emph{SPREAD: a high-resolution daily gridded precipitation dataset for Spain – an extreme events frequency and intensity overview.
#'              Earth Syst. Sci. Data, 9, 721-738, 2017, https://doi.org/10.5194/essd-9-721-2017}
#'
#' @examples
#' data(precp_grid)
"precp_grid"
