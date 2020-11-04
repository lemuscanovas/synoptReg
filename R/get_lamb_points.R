#' Determine the 16 grid points for the Lamb classification
#'
#' Compute the 16 pair of coordinates necessary for using the objective version of the Lamb method
#'
#' @param x longitude coordinate of the central point of the scheme.
#' @param y latitude coordinate of the central point of the scheme.
#'
#' @return a data.frame with the 16 points of coordinates.
#'
#' @examples
#' points <- get_lamb_points(x = -5, y = 40)
#' points
#'
#' @export


get_lamb_points <- function(x,y) {

  xi <- 10
  yi <- 5
  
  
  gp_y <- y - seq(-10,10,by= 5)
  gp_x <- x - c(-15,-5,5,15)
  
  pre_scheme <- expand.grid(gp_y,gp_x) %>%
    setNames(c("y","x"))
  
  corners <- subset(pre_scheme, x == min(x) & y == min(y)|
                     x == min(x) & y == max(y)|
                     x == max(x) & y == min(y)|
                     x == max(x) & y == max(y)) 
  
  jc_scheme <- cbind.data.frame(pre_scheme,
                     TF = interaction(pre_scheme) %in% interaction(corners)) %>%
                       filter(.data$TF == F) %>% 
    select(-.data$TF) %>% 
    cbind.data.frame(c("P6","P10","P14",
              "P2","P5","P9","P13","P16",
              "P1","P4","P8","P12","P15",
              "P3","P7","P11")) %>% 
    setNames(c("lat","lon","label"))
  
  return(jc_scheme)

}


