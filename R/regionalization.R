#' Environmental regionalization
#'
#' Perform an unspervised clustering of a SpatRaster
#'
#' @param x SpatRaster
#' @param centers Integer. Number of clusters.
#' @param iter.max Integer. The maximum number of iterations allowed. Default 100.
#' @param nstart Integer. How many random sets should be chosen? Default 100.
#' @param algorithm Default Lloyd.
#'
#'@return a list with: \itemize{
#'    \item{A list with the results of the K-means performance}
#'    \item{A SpatRaster with the final regionalization }
#'    \item{A list with the results of the K-means performance}
#
#' }
#'
#'
#' @export

regionalization <- function(x, centers, iter.max = 100, nstart = 100, algorithm = "Lloyd") {

    rasters <- x

    nr <- as.data.frame(rasters, cell=TRUE)
    # K-means computation
    set.seed(99)
    
    kmncluster <- kmeans(nr[,-1], centers=centers, 
                         iter.max = iter.max, 
                         nstart = nstart, 
                         algorithm = algorithm)
    
    knr <- rast(rasters, nlyr=1)
    knr[nr$cell] <- kmncluster$cluster
    return(list("cluster_info"=kmncluster,
                "cluster_rast"=knr))
}
