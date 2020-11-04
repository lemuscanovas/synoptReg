library(magrittr)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)


get_lamb_points <- function(x,y) {

  xi <- 10
  yi <- 5
  
  
  gp_y <- y - seq(-10,10,by= 5)
  gp_x <- x - c(-15,-5,5,15)
  
  pre_scheme <- expand.grid(gp_y,gp_x) %>%
    setNames(c("y","x"))
  corners <- filter(pre_scheme, x == min(x) & y == min(y)|
                     x == min(x) & y == max(y)|
                     x == max(x) & y == min(y)|
                     x == max(x) & y == max(y)) 
  
  jc_scheme <- cbind.data.frame(pre_scheme,
                     TF = interaction(pre_scheme) %in% interaction(corners)) %>%
                       filter(TF == F) %>% select(-TF) %>% 
    cbind.data.frame(c("P6","P10","P14",
              "P2","P5","P9","P13","P16",
              "P1","P4","P8","P12","P15",
              "P3","P7","P11")) %>% setNames(c("lat","lon","label"))
  return(jc_scheme)

}



plot_jc_scheme <- function(lamb_points) {

  world <- ne_countries(scale = "medium",returnclass = "sf")
  lamb_points <- lamb_points %>% rename("x"="lon","y"="lat")
  jc_scheme_sf <- st_as_sf(lamb_points,coords = c("x", "y")) %>% st_set_crs(st_crs(world))
  
  pl<- ggplot()+
    geom_sf(data = world, fill = "grey90")+
    geom_sf(data = jc_scheme_sf, size = 3)+
    geom_sf_label(data = jc_scheme_sf, aes(label = label),
                  nudge_y = 1.5, nudge_x = -1.5)+
    theme_bw()+
    scale_x_continuous(limits = c(min(lamb_points$x)-10,max(lamb_points$x)+10))+
    scale_y_continuous(limits = c(min(lamb_points$y)-5,max(lamb_points$y)+5))+
    theme(axis.title = element_blank())
  suppressWarnings(print(pl))
  
}
