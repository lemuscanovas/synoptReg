#' @title classification_jc
#'
#' @description Calculates the classification of the main weather types
#'              for one central point that is surrounded by 16-points (grid16). Wind-flow characteristics
#'              are computed for the daily pressure field according to the rules proposed by the original
#'              Jenkinson and Collison classification (see Jones et al. 1993, Jones et al. 2016).
#' @param mslp  3-Dimensional multi-array ([loni,lati,time]) with mean sea level pressure in Pa.
#' @param grid16   Data frame obtained in the main function (extended_jc) that contains the 16 grid-points defining the scheme.
#'                 First row is for longitudes, while the second row is for latitudes.
#' @param centralp Numeric that refers to the central point for which the JC classification is calculated.
#' @param loni   Array with longitude values.
#' @param lati   Array with latitude values.
#' @param times  Array with the dates used.
#' @param gale   A logical for deteriming Gale days.
#' @return Daily frequencies of Weather Types and airflow indices.
#'
#' @references {
#' Jones, P. D., Hulme M., Briffa K. R. (1993)
#' \emph{A comparison of Lamb circulation types with an objective classification scheme}
#' Int. J. Climatol. 13: 655–663.
#'
#' Jones, P. D., Harpham C, Briffa K. R. (2013)
#' \emph{Lamb weather types derived from Reanalysis products}
#' Int. J. Climatol. 33: 1129–1139.
#' }
#' @seealso  \code{\link{calculate_cwt}}
#' @examples
#' # Load data
#'
#' @export


lamb_clas <- function(points,slp){
  
  var <- vars_lamb(points,slp)
  
  WT <- apply(var,1,lamb_wt)
  
  # clas
  time <- unique(slp$time)
  clas <- tibble(time,WT)
  
  # grid clas
  grid_clas <- inner_join(clas, slp, by = "time") %>% 
    group_by(lon,lat,WT) %>%
    summarise(value) %>% ungroup() %>%
    distinct(lon,lat,WT,.keep_all = T)
  
  return(clas = clas,
         grid_clas = grid_clas)
  
}

vars_lamb <- function(points, slp) {
  
  pp <- inner_join(points, slp, by = c("lon","lat")) %>%
    select(-c(lat,lon)) %>% 
    pivot_wider(names_from = label,values_from = value) 
  
  x<- pp
  
  SF <- 1.305*(0.25*(x$P5+2*x$P9+x$P13)-0.25*(x$P4+2*x$P8+x$P12))
  WF <- (0.5*(x$P12+x$P13)-0.5*(x$P4+x$P5))
  D <- atan(WF/SF)*(360/(2*pi))
  ZS <- 0.85*(0.25*(x$P6+2*x$P10+x$P14)-0.25*(x$P5+2*x$P9+x$P3)-0.25*(x$P4+2*x$P8+x$P12)+0.25*(x$P3+2*x$P7+x$P11))
  ZW <- 1.12*(0.5*(x$P15+x$P16)-0.5*(x$P8+x$P9)-0.91*(0.5*(x$P8+x$P9)-0.5*(x$P1+x$P2)))
  FF <- (SF^2+WF^2)^(1/2)
  Z <- ZS+ZW
  var <- data.frame(SF,WF,D,ZS,ZW,FF,Z)
}  



lamb_wt <- function(x){
  
  dir <- seq(22.5,360,45)
  lev_dir <- levels(cut(seq(0,360,1),seq(22.5,360,45)))
  
  if(abs(x["Z"])<x["FF"]){
    
    out <- ifelse(x["WF"]>0 & x["SF"]>0, x["D"]+180,ifelse(x["WF"]>0 & x["SF"]<0,x["D"]+360,ifelse(x["WF"]<0 & x["SF"]<0,x["D"],x["D"]+180)))
    
    out <- recode(as.character(cut(out,dir)),"(22.5,67.5]"="NE","(67.5,112]"="E","(112,158]"="SE",
                  "(158,202]"="S","(202,248]"="SW","(248,292]"="W","(292,338]"="NW",.missing = "N")
    
  }
  
  
  if(abs(x["Z"])>2*x["FF"]){
    if(x["Z"]>0){
      out <- "C"
    }else{
      out <- "A"
    }
  }
  
  
  if(x["FF"]<abs(x["Z"])&abs(x["Z"])<2*x["FF"]){
    
    if(x["Z"]>0){
      out_p1 <- "C"
    }else{
      out_p1 <- "A"
    }
    
    out_p2 <- unlist(ifelse(x["WF"]>0 & x["SF"]>0, x["D"]+180,ifelse(x["WF"]>0 & x["SF"]<0,x["D"]+360,ifelse(x["WF"]<0 & x["SF"]<0,x["D"],x["D"]+180))))
    
    out_p2 <- recode(as.character(cut(out_p2,dir)),"(22.5,67.5]"="NE","(67.5,112]"="E","(112,158]"="SE",
                     "(158,202]"="S","(202,248]"="SW","(248,292]"="W","(292,338]"="NW",.missing = "N")
    
    
    out <- as.character(paste(out_p1,out_p2,sep=""))
    
  }
  
  
  if(abs(x["Z"])<4.8 & x["FF"]<4.2) out <- "U"
  
  
  ifelse(!is.na(out),return(out),return(NA))
  
}
