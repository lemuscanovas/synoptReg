#' Circulation Types Calculation
#'
#' Based on Lorenzo et al. (2008): Links between circulation weather types and teleconnection
#                                  patterns and their influence on precipitation patterns in Galicia (NW Spain)
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#'
#' @import RNCEP
#' @import dplyr
#' @import tidyr
#' @import lubridate
#'
#' @param points 16 coord point pair in order 1 to 16. More in Details.
#' @param month_range Numeric. Specifies the range of months from each year.
#'  c(1,12) or 1
#' @param year_range Numeric. Specifies the range of years.
#' c(2010,2017) or 2010
#' @param lat_range Numeric. Specifies the range of latitude.
#' c(30,60)
#' @param lon_range Same as lat_range.
#' c(-30,10)
#' @param press_dailymean If you want to the use odaily mean pressue TRUE, or if FALSE you can use a specific hour.
#' @param hour One or several hours of the following:
#' 0,6,12 or 18 c(0,6) or 1
#' @param reanalysis2 default is TRUE. See ?NCEP.gather.
#' @param save_download If TRUE, than the downloaded pressure array is saved as RData file.
#' @param load_data If you already have downloaded data, format has to be the same as the returned array from the download function.
#' @param file_name Name of downloaded data file.
#'
#'
#' @example
#' #For 2017
#' points <- data.frame(long=c(-10,-0,-20,-10,0,10,-20,-10,0,10,-20,-10,0,10,-10,0),
#'                    lat=c(50,50,rep(45,4),rep(40,4),rep(35,4),30,30))
#'
#' cwt <- classjen(points,year_range=2017,save_download=FALSE)
#'
#'
#'
#' @return a data.frame with CT for each requested date and time
#'
#' @export

#pendiente:
#1) en details: debemos indicar diferentes sets de coord 16 puntos para regiones
# alternativa, opcion con varios dataset para elegir
# 2) referencias para esta funcion

#main function
classjen <- function(points,month_range=c(1,12),year_range=c(2010,2017),
                     lat_range=c(30,60),lon_range=c(-30,10),
                     press_dailymean=TRUE,hour=NULL,
                     reanalysis2=TRUE,save_download=TRUE,
                     load_data=FALSE,file_name=NULL,...){

  #download sea level pressure
  pp <- press_download_ncep(points,...)

  #calculation of flows
  var <- classjen_flows(pp)

  #calculation of weather types
  ct <- apply(var,1,classify_jc)

  if(!is.null(hour)){

  wt <- data.frame(date=pp$date,ct=ct)

  }else{

    wt <- data.frame(date=pp$date,hour=pp$hour,ct=ct)

  }

  return(wt)

}




#sea level pressure download

press_download_ncep <- function(points,month_range=c(1,12),year_range=c(2010,2017),
                                lat_range=c(30,60),lon_range=c(-30,10),
                                press_dailymean=TRUE,hour=NULL,
                                reanalysis2=TRUE,save_download=TRUE,
                                load_data=FALSE,file_name=NULL,...){

  if(load_data==FALSE){

    coordpoints <- points

    mat <-  NCEP.gather("slp","surface",
                         month_range,year_range,
                         lat_range,lon_range,
                         reanalysis2 = reanalysis2,
                        return.units=FALSE)
    mat <- mat/100

    if(save_download==TRUE) save(mat,file="press_matrix.RData")

  }else{

    load(file_name)

  }

  pp <- list()

  for(i in 1:16){

    pp[[i]] <- mat[which(as.numeric(dimnames( mat)[[1]])==coordpoints[i,2]),
                     which(as.numeric(dimnames( mat)[[2]])==coordpoints[i,1]),]

  }

  names(pp) <- paste("P",1:16,sep="")

  dates <- names(pp[[1]])

  pp <- bind_cols(pp)

  pp <- mutate(pp,Date_time=ymd_h(dates))

  if(press_dailymean==TRUE){

    pp <- mutate(pp,date=as.Date(Date_time))%>%
            gather(Points,Press,P1:P16)%>%
              group_by(date,Points)%>%
               summarise(press_mean=mean(Press))%>%
           spread(Points,press_mean)%>%as.data.frame()

    return(pp)

  }else{

    pp <- mutate(pp,hour=hour(Date_time))%>%
             filter(hour%in%hour)%>%as.data.frame()

    return(pp)

  }

}


#estimation of flows
classjen_flows <- function(x){

  SF <- 1.350*(0.25*(x$P5+2*x$P9+x$P13)-0.25*(x$P4+2*x$P8+x$P12))
  WF <- (0.5*(x$P12+x$P13)-0.5*(x$P4+x$P5))
  D <- atan(WF/SF)*(360/(2*pi))
  ZS <- 0.85*(0.25*(x$P6+2*x$P10+x$P14)-0.25*(x$P5+2*x$P9+x$P3)-0.25*(x$P4+2*x$P8+x$P12)+0.25*(x$P3+2*x$P7+x$P11))
  ZW <- 1.12*(0.5*(x$P15+x$P16)-0.5*(x$P8+x$P9)-0.91*(0.5*(x$P8+x$P9)-0.5*(x$P1+x$P2)))
  FF <- (SF^2+WF^2)^(1/2)
  Z <- ZS+ZW
  var <- data.frame(SF,WF,D,ZS,ZW,FF,Z)

  return(var)

}


#clasification according to Jenkinson & Collison
classify_jc <- function(x){

  dir <- seq(22.5,360,45)
  lev_dir <- levels(cut(seq(0,360,1),seq(22.5,360,45)))

  if(abs(x["Z"])<x["FF"]){

    out <- case_when(x["WF"]>0 & x["SF"]>0 ~ x["D"]+180,
                     x["WF"]>0 & x["SF"]<0 ~ x["D"]+360,
                     x["WF"]<0 & x["SF"]<0 ~ x["D"],
                     TRUE ~ x["D"]+180)

    out <- recode(as.character(cut(out,dir)),
                  "(22.5,67.5]"="NE",
                  "(67.5,112]"="E",
                  "(112,158]"="SE",
                  "(158,202]"="S",
                  "(202,248]"="SW",
                  "(248,292]"="W",
                  "(292,338]"="NW",
                  .missing = "N")

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

    out_p2 <- unlist(case_when(x["WF"]>0 & x["SF"]>0 ~ x["D"]+180,
                               x["WF"]>0 & x["SF"]<0 ~ x["D"]+360,
                               x["WF"]<0 & x["SF"]<0 ~ x["D"],
                               TRUE ~ x["D"]+180)
                   )

    out_p2 <- recode(as.character(cut(out_p2,dir)),
                     "(22.5,67.5]"="NE",
                     "(67.5,112]"="E",
                     "(112,158]"="SE",
                     "(158,202]"="S",
                     "(202,248]"="SW",
                     "(248,292]"="W",
                     "(292,338]"="NW",
                     .missing = "N")


    out <- as.character(paste(out_p1,out_p2,sep=""))

  }


  if(abs(x["Z"])<4.8 & x["FF"]<4.2) out <- "U"


  ifelse(!is.na(out),return(out),return(NA))

}

