#' Synoptic classification
#'
#' \code{synoptclas} establish a synoptic classification based on any atmospheric variable (i.e. mean sea level pressure, geoptential height at 500 hPa, etc.)
#'
#' @param smode_data Data frame. S-mode data frame of the reanalysis variable. I.e. output obtained from \code{tidy_cuttime_nc} function.
#' @param ncomp Integer. Number of components to be retained.
#' @param extreme_scores Integer. Definition of extreme score threshold (Esteban et al., 2005). Default is 2.
#'
#' @details
#' A PCA is applied to a S-mode matrix to reduce the dimension of the variables, in which the grid points are the variables and the days are the observations.
#' These principal components are subsequently rotated by means of a varimax rotation. With the rotated components, the scores are used to apply the extreme
#' scores method (Esteban et al., 2005). The scores show the degree of representativeness associated with the variation modes of each principal component, i.e.,
#' the classification of each day to its more representative centroid. Thus, the extreme scores method uses the scores > 2 and < -2, establishing a positive and
#' negative phase for each principal component. The extreme scores procedure establishes the number of groups and their centroids in order to apply the K-means
#' method without iterations.
#'
#' @return A list with: \itemize{
#'    \item{A data frame containing data grouped by circulation weather types ("grouped_data").}
#'    \item{An integer with the circulation weather types ("clas").}
#'    \item{A data frame containing the number and percentage of days assigned to each weather type ("cwt_freq").}
#'    \item{A data frame containing the number of days assigned to each weather type by month ("monthly_freq").}
#'    \item{A data frame containing the number of days assigned to each weather type by year ("annual_freq").}
#'    \item{The 'rotated' loadings matrix of class \code{"loadings"} ("rotated_loadings").}
#'    \item{The scores of the supplied data on the principal components ("scores").}
#'    \item{The coordinates of the scores used to perform the k-means clustering. For more information, read Esteban et al. (2005)("scores_coordinates").}
#' }
#'
#' @examples
#' # Load data (mslp)
#' data(mslp)
#' # Converting our data into a S-mode, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
#' # classification performance
#' smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
#'
#'
#' @references {
#' Esteban, P. , Jones, P. D., Martín‐Vide, J. and Mases, M. (2005)
#' \emph{Atmospheric circulation patterns related to heavy snowfall days in Andorra, Pyrenees}
#' Int. J. Climatol. 25: 319-329. doi:10.1002/joc.1103
#'}
#' @seealso  \code{\link{pca_decision}}
#'
#' @export

synoptclas <- function(smode_data, ncomp, extreme_scores = 2) {

    message("Executing synoptic classification...")
    pca <- princomp(smode_data, cor = T, scores = T)

    # Computing varimax rotation and scores calculation
    rawLoadings <- pca$loadings[, 1:ncomp] %*% diag(pca$sdev, ncomp, ncomp)
    rotatedLoadings <- varimax(rawLoadings)$loadings
    scores <- scale(pca$scores[, 1:ncomp]) %*% varimax(rawLoadings)$rotmat
    scores <- as.data.frame(scores)

    # Extreme scores calculation (Esteban et al., 2005)
    list_extreme_scores <- list()

    for (ii in 1:ncomp) {
        tt <- seq(1:ncomp)
        subdata <- scores[apply(scores[, c(tt[-ii])] < extreme_scores,
                                1, all), ]
        subdata2 <- subdata[apply(subdata[, c(tt[-ii])] > -extreme_scores,
                                  1, all), ]
        subdata_pos <- colMeans(subdata2[apply(subdata2[ii] > extreme_scores,
                                               1, all),])
        subdata_neg <- colMeans(subdata2[apply(subdata2[ii] < -extreme_scores,
                                               1, all),])
        result <- rbind(subdata_pos, subdata_neg)
        rownames(result) <- c(paste0("pc", ii, "+"), paste0("pc", ii, "-"))
        list_extreme_scores[[ii]] <- result
    }

    # scores coordinates
    coordinates_scores <- do.call(rbind.data.frame, list_extreme_scores)
    coordinates_scores <- na.omit(coordinates_scores)

    # kmeans classification
    clas <- suppressWarnings(kmeans(scores, centers = coordinates_scores,
                                    iter.max = 1))

    count <- c()
    perc <- list()
    for (ii in seq(min(clas$cluster):max(clas$cluster))) {
        count[ii] <- sum(clas$cluster == ii)
        perc[ii] <- sum(clas$cluster == ii)/sum(clas$cluster != 0) * 100
    }

    cwt_total_frequency <- rbind.data.frame(count, perc)
    colnames(cwt_total_frequency) <- paste0("cl", 1:length(count))
    rownames(cwt_total_frequency) <- c("counts", "percent")

    dates <- as.Date(rownames(smode_data))
    months <- format(as.Date(dates), "%m")
    years <- format(as.Date(dates), "%Y")

    # creating dataframe of monthly frequencies per CWT
    histo_month <- raster::aggregate(clas$cluster, by = list(Month = format(dates, "%m"), clas$cluster),
        sum, na.rm = TRUE)
    monthly_frequency <- histo_month[, 3]/histo_month[, 2]
    histo_month[, 3] <- NULL
    histo_month$frequency <- monthly_frequency
    colnames(histo_month) <- c("Month", "CWT", "Counts")

    # creating dataframe of yearly frequencies per CWT
    histo_annual <- raster::aggregate(clas$cluster, by = list(Year = format(dates, "%Y"), clas$cluster),
        sum, na.rm = TRUE)
    annual_frequency <- histo_annual[, 3]/histo_annual[, 2]
    histo_annual[, 3] <- NULL
    histo_annual$frequency <- annual_frequency
    colnames(histo_annual) <- c("Year", "CWT", "Counts")

    # Binding dates, CWT and data to plot after
    final_data <- cbind.data.frame(dates, clas$cluster, smode_data)
    colnames(final_data)[2] <- "CWT"

    return(list(grouped_data = final_data, clas = clas$cluster, cwt_freq = cwt_total_frequency,
        monthly_freq = histo_month, annual_freq = histo_annual, rotated_loadings = rotatedLoadings,
        scores = scores, scores_coordinates = coordinates_scores))

}
