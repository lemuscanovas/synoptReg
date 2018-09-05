#' Raster PCA
#'
#' Perform a Principal Component Analysis on a RasterStack
#'
#' @param raststack Raster Stack.
#' @param aggregate Integer. Aggregation factor based on function \code{aggregate} of \pkg{raster} package.
#' @param focal Integer. smooth filter based on function \code{focal} of \pkg{raster} package.
#'
#' @return a list with: \itemize{
#'    \item{A raster stack containing the results of the PCA }
#'    \item{A data frame containing the main results of the PCA (standard deviation, proportion of variance and cumulative variance}
#' }
#'
#' @export


raster_pca <- function(raststack, aggregate = 0, focal = 0) {

    # Computing PCA
    rasterPCA <- princomp(na.omit(raster::values(raststack)), cor = T,
                          scores = T)
    resultPCA <- raster::predict(rasterPCA, raststack[])

    # Storing all rasters in a raster stack
    stackrasterPCA <- raster::stack()

    for (ii in 1:raster::nlayers(raststack)) {
        r2 <- raster::raster(raststack[[1]])
        r2[] <- resultPCA[, ii]
        stackrasterPCA <- raster::stack(stackrasterPCA, r2)
    }
    names(stackrasterPCA) <- paste0("PC", 1:raster::nlayers(raststack))

    # Summary of PCA results
    summaryPCA <- as.data.frame(rbind(SD <- sqrt(rasterPCA$sdev^2),
                                      proportion <- rasterPCA$sdev^2/
                                        sum(rasterPCA$sdev^2),
                                      cumulative <- cumsum(rasterPCA$sdev^2)/
                                        sum(rasterPCA$sdev^2)))
    rownames(summaryPCA) <- c("sdev", "prop.variance", "cum.variance")

    # Focal function
    multiFocal <- function(x, w = matrix(1, nrow = 3, ncol = 3), ...) {

        if (is.character(x)) {
            x <- raster::brick(x)
        }
        # The function to be applied to each individual layer
        fun <- function(ind, x, w, ...) {
            raster::focal(x[[ind]], w = w, ...)
        }

        n <- seq(raster::nlayers(x))
        list <- lapply(X = n, FUN = fun, x = x, w = w, ...)

        out <- raster::stack(list)
        return(out)
    }

    # Aggregating our rasters (resample)
    if (aggregate != 0) {
        aggregatePCA <- raster::aggregate(stackrasterPCA, fact = aggregate)
        names(aggregatePCA) <- paste0("PC", 1:raster::nlayers(raststack))

        if (focal != 0) {
            aggfiltPCA <- multiFocal(aggregatePCA, w = matrix(1, focal, focal),
                                     fun = "mean", na.rm = T)
            names(aggfiltPCA) <- paste0("PC", 1:raster::nlayers(raststack))

            return(list(rasterPCA = aggfiltPCA, summary = summaryPCA))

        } else {

            return(list(rasterPCA = aggregatePCA, summary = summaryPCA))

        }

    } else {
        if (focal != 0) {
            filtPCA <- multiFocal(stackrasterPCA, w = matrix(1, focal, focal),
                                  fun = "mean", na.rm = T)
            names(filtPCA) <- paste0("PC", 1:raster::nlayers(raststack))

            return(list(rasterPCA = filtPCA, summary = summaryPCA))

        } else {

            return(list(rasterPCA = stackrasterPCA, summary = summaryPCA))
        }
    }
}
