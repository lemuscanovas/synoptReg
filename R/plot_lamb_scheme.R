#' Plot Lamb Scheme
#'
#' Visualize the Lamb Scheme
#'
#' @param points points obtained from the \code{get_lamb_points} function.
#'
#' @return a ggplot map.
#'
#' @examples
#' points <- get_lamb_points(x = -5, y = 40)
#' 
#' plot_lamb_scheme(points)
#' 
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import ggplot2
#' @importFrom sf st_as_sf st_set_crs st_crs
#'
#' @export

plot_lamb_scheme <- function(points) {
  
  world <- ne_countries(scale = "medium",returnclass = "sf")
  lamb_points <- points %>% rename("x"="lon","y"="lat")
  jc_scheme_sf <- st_as_sf(lamb_points,coords = c("x", "y")) %>% st_set_crs(st_crs(world))
  
  pl<- ggplot()+
    geom_sf(data = world, fill = "grey90")+
    geom_sf(data = jc_scheme_sf, size = 3)+
    geom_sf_label(data = jc_scheme_sf, aes(label = .data$label),
                  nudge_y = 1.5, nudge_x = -1.5)+
    theme_bw()+
    scale_x_continuous(limits = c(min(lamb_points$x)-10,max(lamb_points$x)+10))+
    scale_y_continuous(limits = c(min(lamb_points$y)-5,max(lamb_points$y)+5))+
    theme(axis.title = element_blank())
  suppressWarnings(print(pl))
  
}
