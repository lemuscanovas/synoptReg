#' Environmental regionalisation
#'
#' Perform an unspervised clustering of the Raster Stack
#'
#' @param raststack Raster Stack.
#' @param centers Integer. Number of clusters.
#' @param iter.max Integer. The maximum number of iterations allowed. Default 100.
#' @param nstart Integer. How many random sets should be chosen? Default 100.
#'
#'@return a list with: \itemize{
#'    \item{A raster with the final regionalisation }
#'    \item{A list with the results of the K-means performance}
#'    \item{A raster displaying a pseudo-MAE error based on the difference between each pixel value and its respective centroide}
#'    \item{A numeric pseudo-MAE mean value for the entire map}
#' }
#'
#' @export

regionalisation <- function(raststack, centers, iter.max = 100, nstart = 100) {

    rasters <- raster::stack(raststack)
    rasters <- raster::brick(rasters)
    valuetable <- raster::getValues(rasters)
    rNA <- raster::setValues(raster::raster(rasters), 0)

    # K-means computation
    km <- suppressWarnings(kmeans(na.omit(valuetable), centers = centers,
                                  iter.max = iter.max, nstart = nstart))

    for (i in 1:raster::nlayers(rasters)) {
        rNA[is.na(rasters[[i]])] <- 1
    }

    rNA <- raster::getValues(rNA)

    valuetable <- as.data.frame(valuetable)
    valuetable$class[rNA == 0] <- km$cluster
    valuetable$class[rNA == 1] <- NA

    classes <- raster::raster(rasters)

    classes <- raster::setValues(classes, valuetable$class)
    classes_rm <- classes
    centers <- rowMeans(km$centers)  #PCA centroids mean
    errormap <- raster::stack()
    for (ii in 1:length(centers)) {
        classes_rm[classes_rm == ii] <- centers[ii]
    }

    # Pseudo-MAE raster
    errormap <- raster::stack(errormap, abs(classes_rm - raster::mean(rasters))
                              /raster::ncell(rasters))

    # Pseudo-MAE mean value
    mae_mean <- errormap.mean <- raster::cellStats(errormap, "mean")

    return(list(regionalisation = classes, kmeans = km, errormap = errormap,
                pseudoMAE = mae_mean))
}
