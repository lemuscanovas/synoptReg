#' Raster PCA
#'
#' Perform a Principal Component Analysis on a SpatRaster
#'
#' @param x SpatRaster.
#' @param aggr Integer. Aggregation factor based on function \code{aggregate} of \pkg{terra} package.
#' @param focal Integer. smooth filter based on function \code{focal} of \pkg{terra} package.
#'
#' @return a list with: \itemize{
#'    \item{A SpatRaster containing the results of the PCA }
#'    \item{A data.frame containing the main results of the PCA (standard deviation, proportion of variance and cumulative variance}
#' }
#'
#' @importFrom terra spatSample ncell aggregate focal time
#' @importFrom stats predict na.omit

#' @export


raster_pca <- function(x, aggr = NULL, focal = NULL){
  # Ensure input is a SpatRaster
  if(!is(x, "SpatRaster")) {
    stop("Input should be a SpatRaster object")
  }
  
  pca <- prcomp(na.omit(spatSample(x,ncell(x),"regular")))
  eigs <- pca$sdev^2
  info_variance <- rbind(
    SD = sqrt(eigs),
    Proportion = eigs/sum(eigs),
    Cumulative = cumsum(eigs)/sum(eigs))
  
  pca_predict <- predict(x, pca)
 
  names(pca_predict) <- paste0("PC", 1:nlyr(pca_predict))
  
  if(!is.null(aggr)){
    pca_predict <- aggregate(pca_predict,aggr) 
    
  }
  
  if(!is.null(focal)){
    pca_predict <- focal(pca_predict, w=focal, fun="mean") 
    
  }
  
return(list("PCA" = pca_predict,
            "summaryPCA" = info_variance))
}

