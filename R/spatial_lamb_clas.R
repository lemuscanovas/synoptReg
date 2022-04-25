#' @title Spatial objective Lamb Weather Type Classification
#'
#' @description Calculates the  automatic Lamb or Jenkinson and Collison classification at each grid point. 
#' The approach details are described in  \code{\link{lamb_clas}}.
#' @param mslp  Mean Sea Level pressure gridded data.
#' @param xmin  minimum longitude
#' @param xmax  maximum longitude
#' @param ymin  minimum latitude
#' @param ymax  maximum longitude
#' @param U Logical. If T, Jones et al. 2013 approach is applied, maintaining the U-type in the classification. If F, U is removed as detailed in Trigo and DaCamara, 2000.
#' @param thr threshold used for Unclassified days (total shear vorticity and total flow, respectively). Default c(6,6).
#' @param cores Number of cores to be used
#' 
#' @return A list with: \itemize{
#'    \item{A data frame containing the gridded  classification}
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
#' 
#' Trigo, R., DaCamara C. (2000)
#' \emph{Circulation weather types and their impact on the precipitation regime in Portugal}
#' Int. J. Climatol. 20: 1559-1581.
#' 
#' @seealso  \code{\link{lamb_clas}}
#' 
#' @examples
#' data(mslp)
#' 
#' clas <- spatial_lamb(mslp, xmin = 5,xmax = 15, ymin = 40, ymax = 50, cores = 1)
#'
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom future.apply future_apply
#' @importFrom future plan multisession
#'
#' @export

spatial_lamb <- function(mslp, xmin = -180, xmax = 180 , ymin = -80, ymax = 80, 
                         U = T, thr = c(6,6), cores = 1 ){
  
  lons = seq(xmin,xmax,2.5)
  lats = seq(ymin,ymax,2.5)
  
  grid <- expand.grid(lons, lats) %>%
    filter(.data$Var1 !=-180)
  # get lamb points to all grid
  points <- apply(grid,
                  MARGIN = 1,
                  function(x) get_lamb_points_helper(x[1], x[2]))
  
  # -180 doesn't exist in NCEP NCAR
  
  # points <- purrr::discard(points, ~any(.x$lon == -180))
  
  lat_max_required <- max(sapply(points, function(x) max(x[[1]])))
  lat_min_required <- min(sapply(points, function(x) min(x[[1]])))
  lon_max_required <- max(sapply(points, function(x) max(x[[2]])))
  lon_min_required <- min(sapply(points, function(x) min(x[[2]])))
  
  if (max(mslp$lon) < lon_max_required | min(mslp$lon) > lon_min_required |
      max(mslp$lat) < lat_max_required | min(mslp$lat) > lat_min_required)
    
    stop(paste0('the mslp dataset has a smaller extent than required.\n
         Your mslp extension (xmin, xmax, ymin, ymax):',
                min(mslp$lon),",",
                max(mslp$lon),",",
                min(mslp$lat),",",
                max(mslp$lat)),"\n
         The required extension (xmin, xmax, ymin, ymax):",
         lon_min_required,",",
         lon_max_required,",",
         lat_min_required,",",
         lat_max_required)
  else{
    
    plan(multisession,workers = cores) ## Run in parallel on local computer
    
    
    vars <- future.apply::future_lapply(points, FUN = lamb_clas_helper, mslp = mslp, thr = thr, U = U)
    clas <- bind_cols(lon = rep(grid$Var1, each = length(unique(mslp$time))),
                      lat = rep(grid$Var2, each = length(unique(mslp$time))),
                      cl = bind_rows(vars))
    
    clas$WT <- as.factor(clas$WT)
    clas$WT <- factor(clas$WT, levels = c("A","ANE","AE","ASE","AS","ASW","AW","ANW","AN",
                                          "NE","E","SE","S","SW","W","NW","N",
                                          "C","CNE","CE","CSE","CS","CSW","CW","CNW","CN", 
                                          "U"))
    return(clas)
  }
}

get_lamb_points_helper <- function(x,y) {
  
  xi <- 10
  yi <- 5
  
  
  gp_y <- y - seq(-10,10,by= 5)
  gp_x <- x - c(-15,-5,5,15)
  gp_x <- ifelse(gp_x < -177.5, gp_x +360,gp_x)
  gp_x <- ifelse(gp_x > 180, gp_x -360,gp_x)
  
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

lamb_clas_helper <- function(points,mslp = mslp, U, thr){
  
  var <- vars_lamb_helper(points,mslp,U)
  
  WT <- apply(var,1,lamb_wt_helper,U,thr)
  
  # clas
  time <- unique(mslp$time)
  clas <- tibble(time,WT)
  
  
  return(clas)
  
}


vars_lamb_helper <- function(points, mslp,U) {
  
  pp <- inner_join(points, mslp, by = c("lon","lat")) %>%
    select(c(.data$label,.data$time,.data$value)) %>% 
    pivot_wider(names_from = .data$label,values_from = .data$value) 
  x<- pp
  
  if(U == F){ #Trigo & DaCamara, 2000
    SF <- 1.305*(0.25*(x$P5+2*x$P9+x$P13)-0.25*(x$P4+2*x$P8+x$P12))
    WF <- (0.5*(x$P12+x$P13)-0.5*(x$P4+x$P5))
    D <- atan(WF/SF)*(360/(2*pi))
    ZS <- 0.85*(0.25*(x$P6+2*x$P10+x$P14)-0.25*(x$P5+2*x$P9+x$P3)-0.25*(x$P4+2*x$P8+x$P12)+0.25*(x$P3+2*x$P7+x$P11))
    ZW <- 1.12*(0.5*(x$P15+x$P16)-0.5*(x$P8+x$P9)-0.91*(0.5*(x$P8+x$P9)-0.5*(x$P1+x$P2)))
    FF <- (SF^2+WF^2)^(1/2)
    Z <- ZS+ZW
    
  } else { # Jones et al., 1993
    SF <- 1.74*(0.25*(x$P5+2*x$P9+x$P13)-0.25*(x$P4+2*x$P8+x$P12))
    WF <- 0.5*(x$P12+x$P13)-0.5*(x$P4+x$P5)
    D <- atan(WF/SF)*(360/(2*pi))
    ZS <- 1.52*(0.25*(x$P6+2*x$P10+x$P14)-0.25*(x$P5+2*x$P9+x$P3)-0.25*(x$P4+2*x$P8+x$P12)+0.25*(x$P3+2*x$P7+x$P11))
    ZW <- 1.07*(0.5*(x$P15+x$P16)-0.5*(x$P8+x$P9)-0.95*(0.5*(x$P8+x$P9)-0.5*(x$P1+x$P2)))
    FF <- (SF^2+WF^2)^(1/2)
    Z <- ZS+ZW
    
  }
  var <- data.frame(SF,WF,D,ZS,ZW,FF,Z)
}  



lamb_wt_helper <- function(x,U,thr){
  
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
  
  # Only works when U = TRUE. 
  if(U!= FALSE & abs(x["Z"])<thr[1] & x["FF"]<thr[2]) out <- "U"
  
  
  ifelse(!is.na(out),return(out),return(NA))
  
}

