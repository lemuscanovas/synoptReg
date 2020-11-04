#' @title Objective Lamb Weather Type Classification
#'
#' @description Calculates the classification of the main weather types
#'              for the 16-points defined in \code{get_lamb_points}. Wind-flow characteristics
#'              are computed for the daily pressure field according to the rules proposed by the original
#'              Jenkinson and Collison classification (see Jenkin-son and Collison, 1977, Jones et al. 1993, Jones et al. 2016).
#' @param points  16 point pair of coordinates obtained from \code{get_lamb_points}.
#' @param slp  Mean Sea Level pressure gridded data.
#' @return A list with: \itemize{
#'    \item{A data.frame containing the dates and the weather types.
#'    \item{A data frame containing the gridded data grouped by circulation types.
#' }
#' 
#' @references {
#' Jenkinson, A.F., Collison F.P (1977)
#' \emph{An initial climatology of gales over the North Sea}
#' Synoptic Climatology Branch Memorandum, No. 62.Meteorological Office: Bracknell, England.
#' 
#' Jones, P. D., Hulme M., Briffa K. R. (1993)
#' \emph{A comparison of Lamb circulation types with an objective classification scheme}
#' Int. J. Climatol. 13: 655–663.
#'
#' Jones, P. D., Harpham C, Briffa K. R. (2013)
#' \emph{Lamb weather types derived from Reanalysis products}
#' Int. J. Climatol. 33: 1129–1139.
#' }
#' @seealso  \code{\link{get_lamb_points}}
#' @examples
#'
#' @importFrom tidyr pivot_wider pivot_longer 
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
    group_by(.data$lon,.data$lat,.data$WT) %>%
    summarise(.data$value) %>% ungroup() %>%
    distinct(.data$lon,.data$lat,.data$WT,.keep_all = T)
  
  return(list(clas = clas,
         grid_clas = grid_clas))
  
}

vars_lamb <- function(points, slp) {
  
  pp <- inner_join(points, slp, by = c("lon","lat")) %>%
    select(-c(.data$lat,.data$lon)) %>% 
    pivot_wider(names_from = .data$label,values_from = .data$value) 
  
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
