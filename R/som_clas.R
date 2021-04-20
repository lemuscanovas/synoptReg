#' Self-Organized Map classification
#'
#' \code{som_clas} allows to perform a SOM synoptic classification 
#'
#' @param x data.frame. A data.frame with the following variables: \code{lon, lat, time, value, anom_value}. See \code{tidy_nc}.
#' @param xdim Integer. X dimension of the grid. See \code{somgrid} from \code{kohonen} package.
#' @param ydim Integer. Y dimension of the grid. See \code{somgrid} from \code{kohonen} package.
#' @param iter integer. Number of iterations.
#' @param alpha vector. learning rate. See \code{som} from \code{kohonen} package for details.
#' @param dist.fcts character. vector of distance functions to be used for the individual data layers. See \code{som} from \code{kohonen} package for details.
#' @param mode carachter. type of learning algorithm. Default "on-line". See \code{kohonen} package for details.
#' @param cores Integer. Parallel processing only available for "pbatch" algorithm.
#' @param norm logical. Default \code{TRUE}. \code{norm = TRUE} is recommended for classifying two ore more variables.
#'
#'
#' @return A list with: \itemize{
#'    \item{A data.frame containing the dates and the weather types.}
#'    \item{A data frame containing the gridded data grouped by circulation types.}
#'    \item{An object of class \code{kohonen} with all the components returned by the function \code{som}}
#' }
#'
#' @examples
#' # Load data
#' data(z500)
#' # Tidying our atmospheric variables (500 hPa geopotential height).
#' z500_tidy <- tidy_nc(x = list(z500),
#'              name_vars = c("z500"))
#'
#' # SOM classification
#' som_cl <- som_clas(z500_tidy, xdim = 4, ydim = 4, iter = 200)
#'
#'
#' @references {
#' Wehrens, R. and BuydenL. (2007)
#' \emph{Self- and Super-organizing Maps in R: The kohonen Package}
#' Journal of Statistical Software, 21(5), 1 - 19.
#'}
#' @seealso  \code{\link{tidy_nc}}
#'
#' @importFrom kohonen som somgrid
#'
#' @export

som_clas <- function(x, xdim, ydim,iter = 2000, alpha = c(0.05,0.01), 
                     dist.fcts = "euclidean", mode = "online",
                     cores = 1, norm = T) {
  
  dataset <- x %>% 
    select(-.data$anom_value) %>%
    group_by(.data$var, .data$lon, .data$lat) %>%
    mutate(value = scale(.data$value)) %>%
    unite("varlonlat",.data$var:.data$lat, sep = "_") %>%
    tidyr::pivot_wider(names_from = .data$varlonlat, values_from= .data$value)
  
  
  # Transforming into a matrix
  matrix_vars <- dataset %>% dplyr::ungroup() %>%
    dplyr::select(-.data$time) %>% as.matrix()
  
  # Creating a SOM network
  
  WTs = xdim*ydim
  
  som_vars <- som(X = matrix_vars,
                  grid = somgrid(xdim = xdim,
                                 ydim = ydim,
                                 neighbourhood.fct = "gaussian",
                                 topo = "rectangular"),
                  rlen = iter,
                  alpha = alpha,
                  keep.data = T,
                  dist.fcts = dist.fcts,
                  mode = mode,
                  cores = cores)
  
  
  grid_som_vars <- som_vars$codes[[1]] %>% 
    as.data.frame() %>% as_tibble() %>%
    mutate(xdim = rep(1:xdim,xdim),
           ydim = rep(1:ydim, each = ydim),
           WT = 1:WTs) %>%
    relocate(.data$ydim, .before = 1) %>%
    relocate(.data$xdim, .before = .data$ydim) %>%
    relocate(.data$WT, .before = .data$xdim) %>%
    pivot_longer(4:ncol(.),names_to = "grid") %>%
    separate(col = .data$grid, into = c("var","lon","lat"),sep = "_") %>%
    mutate_at(.vars = vars(.data$lon,.data$lat), .funs = as.numeric) %>%
    select(.data$var,.data$lon,.data$lat,.data$WT, .data$xdim, .data$ydim)
  
  clas <- tibble(time = unique(x$time), WT = som_vars$unit.classif)
  
  df_classified <- x %>%
    inner_join(clas, by = "time") %>%
    inner_join(grid_som_vars, by = c("var","lon","lat","WT"))
  
  df_classified_panels <- df_classified %>%
    group_by(.data$lon, .data$lat, .data$WT, .data$var) %>%
    mutate(mean_WT_value = mean(.data$value),
           mean_WT_anom_value = mean(.data$anom_value),
           pval_ttest = t.test(.data$anom_value, mu = 0)$p.value,
           cv_WT_value = (sd(.data$value) / mean(.data$value)) * 100) %>%
    select(-.data$value, -.data$anom_value) %>%
    ungroup() %>%
    distinct(.data$lon, .data$lat, .data$WT, .data$var, .keep_all = T)
  
  return(list(clas = clas, 
              grid_clas = df_classified_panels,
              som_info = som_vars))
}
