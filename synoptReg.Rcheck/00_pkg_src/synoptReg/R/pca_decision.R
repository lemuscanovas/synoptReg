#' PCA decision
#'
#' \code{pca_decision} plots the explained variances against the number of the principal component. In addition, it returns all the information about the PCA performance.
#'
#' @param smode_data S-mode dataframe of the reanalysis variable. I.e. output obtained from \code{tidy_cuttime_nc} function.
#'
#' @return a list with: \itemize{
#'    \item{A list with class \code{"princomp"} containing all the results of the PCA }
#'    \item{A data frame containing the main results of the 30 first PCA (standard deviation, proportion of variance and cumulative variance).}
#' }
#'
#' @note S-mode PCA require more rows than columns to work. In addition, input data cannot contain NAs.
#'
#' @examples
#' # Load data (mslp)
#' data(mslp)
#' # Converting our data into a S-mode, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
#' # PCA decision performance
#' info_pca <- pca_decision(smode_mslp$smode_data)
#'
#' @seealso
#' \code{\link{tidy_cuttime_nc}}
#' @export
#' @importFrom graphics grid
#' @importFrom stats kmeans na.omit princomp varimax



pca_decision <- function(smode_data) {

    if (sum(is.na(smode_data)) == 0) {
        pca <- princomp(smode_data, cor = T)

        # summary table
        summaryPCA <- as.data.frame(rbind(SD <- sqrt(pca$sdev^2),
                                    proportion <- pca$sdev^2/
                                      sum(pca$sdev^2),
                                    cumulative <- cumsum(pca$sdev^2)/
                                      sum(pca$sdev^2)))
        summaryPCA <- summaryPCA[, 1:30]  #30 first PCA

        x <- NULL
        for (i in 1:30) {
            x[i] <- paste0("comp.", i)
        }
        colnames(summaryPCA) <- x
        rownames(summaryPCA) <- c("sdev", "prop.variance", "cum.variance")

        # plot scree test
        apply(summaryPCA[2, ], MARGIN = 1, FUN = graphics::plot, type = "b",
              main = "Scree Test", xlab = "Components",
              ylab = "explained variance")
        grid()

    } else {
        stop("Impossible to execute if data contains NA")
    }

    return(list(PCA = pca, summary = summaryPCA))
}
