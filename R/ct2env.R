#' Establishing the relationship between CT and a environmental variable
#'
#' This function applies the approach: "circulation types to environment".
#'
#' @param x data.frame. A data.frame containing the environmental data (i.e. precipitation, temperature, PM10, etc.) with the following variables: \code{lon, lat, time, value, anom_value}. See \code{tidy_nc}.
#' @param clas data.frame. A data.frame of the synoptic classification (time and WT) obtained from the \code{synoptclas} function.
#' @param fun function. A function to be applied to the environmental variable for each WT.
#' @param out character. Choose between \code{"data.frame"} (default) or \code{"SpatRaster"} A function to be applied to the environmental variable for each WT.
#'
#' @return a data.frame or a Raster Stack containing the environmental grids based on the weather types.
#'
#' @examples
#' \dontrun{
#' # Load atmospheric data
#' data(msl)
#' data(z500)
#'
#' # Joining both variables
#' atmos_data <- dplyr::bind_rows(msl,z500)
#'
#' # S-mode classification
#' smode_cl <- synoptclas(atmos_data, ncomp = 6, norm = T)
#'
#' # Load precipitation data 
#' pcp_file <- system.file("inst/extdata", "pcp_spread.nc", package = "synoptReg")
#' # ct2env (precipitation example)
#' ct2env(x = pcp, clas = smode_cl$clas, fun = mean, out = "data.frame")
#'}
#'
#' @importFrom terra tapp
#' @export

ct2env <- function(x, clas, fun = mean, out = "data.frame") {
  
    FUN <- match.fun(fun)
    dates_env <- terra::time(x)
    dates_clas <- clas$time
    
    if(length(dates_env) != length(dates_clas)){
    ## Time series matching if they have different lengths
    match_dates_env <- which(dates_env %in% dates_clas)
    x <- x[[match_dates_env]]
    
    match_dates_WT <- which(dates_clas %in% time(x))
    clas <- slice(clas, match_dates_WT)
    }
    WTs <- select(clas,2) %>% pull %>% as.factor()
    
    seq_wts <- WTs %>% unique() %>% sort %>% as.character()
   
    env <- tapp(x, WTs, FUN)
    names(env) <- seq_wts
    
    # env <- env[[order(names(env))]]
    varnames(env) <- varnames(x)
    units(env) <- unique(units(x))
    names(env)<- seq_wts
    
    if (out == "SpatRaster") {

        return(env)

    } else if (out == "data.frame") {
  
        env_df <- env %>%
            as.data.frame(xy = T) %>%
            as_tibble() %>%
            pivot_longer(names_to = "WT",values_to = varnames(env), 3:ncol(.)) %>%
            mutate(units = unique(units(env)))
        return(env_df)
    }
}
