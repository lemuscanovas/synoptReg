#' PCA Synoptic classification
#'
#' \code{synoptclas} allows to perform several types of synoptic classification approaches based on one or several atmospheric variables (i.e. mean sea level pressure, geoptential height at 500 hPa, etc.)
#'
#' @param x data.frame. A data.frame with the following variables: \code{lon, lat, time, value, anom_value}. See \code{tidy_nc}.
#' @param ncomp Integer. Number of components to be retained.
#' @param norm logical. Default \code{TRUE}. \code{norm = TRUE} is recommended for classify two ore more variables.
#' @param matrix_mode character. The mode of matrix to use. Choose between S-mode and T-mode
#' @param extreme_scores Integer. Definition of extreme score threshold (Esteban et al., 2005). Default is 2. Only applicable for a \code{matrix_mode = "S-mode"}
#'
#' @details
#' The \code{matrix_mode} argument allows to conduct different types of synoptic classifications depending on the user's objective.
#' If the user wants to perform a synoptic classification of a long and continuous series, he must set the \code{matrix_mode = "S-mode"}.
#' When we apply the PCA to a matrix in S-mode, the variables are the grid points (lon,lat) and the observations are the days (time series),
#' so the linear relationships that the PCA establishes are between the time series of the grid points. One of the results we obtain from the PCA are the "scores",
#' which indicate the degree of representativeness of each day for each of the principal components. However, the scores do not allow us to directly obtain the
#' weather types (WT) classification, since one day can be represented by several principal components. For this reason, a clustering method is required  to group each day
#' to an specific WT based on the multivariate coordinates provided by the "scores". Before using a clustering method, a VARIMAX rotation is performed on the principal Components
#' retained, with the aim of redistributing the variance of such components.
#' With the rotated components, the scores are used to apply the extreme scores method (Esteban et al., 2005). The scores show the degree of representativeness associated
#' with the variation modes of each principal component, i.e., the classification of each day to its more representative centroid. Thus, the extreme scores method uses
#' the scores > 2 and < -2, establishing a positive and negative phase for each principal component. The extreme scores procedure establishes the number of groups and their
#' centroids in order to apply the K-means method without iterations.
#' Conversely, if the user wants to perform a synoptic classification of specific events (i.e. flood events, extreme temperatures events,etc.), he must set the \code{matrix_mode = "T-mode"}.
#' In this case, the variables are the days (time series) and the observations are the grid points. The relationships established in this case are between each daily gridded map.
#' For this reason, the eigenvalues (correlations) allow to allow us to associate each day to a WT without using a clustering method as in the case of the S-mode matrix.
#'
#' @return A list with: \itemize{
#'    \item{A data.frame containing the dates and the weather types. If "T-mode" is selected, two classifications are returned (absolute and positive/negative classification).}
#'    \item{A data frame containing the gridded data grouped by circulation types.If "T-mode" is selected, 3 classifications are returned (absolute correlation,maximum positive correlation, and positive/negative classification) .}
#' }
#'
#' @examples
#' # Load data (mslp or precp_grid)
#' data(mslp)
#' data(z500)
#' # Tidying our atmospheric variables (500 hPa geopotential height
#' # and mean sea level pressure) together.
#'
#' # Time subset between two dates
#' atm_data1 <- tidy_nc(x = list(mslp,z500),
#'              name_vars = c("mslp","z500"))
#'
#' # S-mode classification
#' smode_clas <- synoptclas(atm_data1, ncomp = 6)
#'
#' # Time subset using a vector of dates of interest
#' dates_int <- c("2000-01-25","2000-04-01","2000-07-14","2001-05-08","2002-12-20")
#' atm_data2 <- tidy_nc(x = list(mslp,z500),
#'                      time_subset = dates_int,
#'                      name_vars = c("mslp","z500"))
#'
#' # S-mode classification
#' tmode_clas <- synoptclas(atm_data2, ncomp = 2, matrix_mode = "T-mode")
#'
#'
#' @references {
#' Esteban, P. , Jones, P. D., Martin.Vide, J.
#' \emph{Atmospheric circulation patterns related to heavy snowfall days in Andorra, Pyrenees}
#' Int. J. Climatol. 25: 319-329. doi:10.1002/joc.1103
#'}
#' @seealso  \code{\link{pca_decision}}
#'
#' @importFrom stats loadings sd setNames time
#'
#' @export

synoptclas <- function(x, ncomp, norm = T, matrix_mode = "S-mode", extreme_scores = 2) {

    if (matrix_mode == "S-mode") {

        if (norm == T) {
            pca <- x %>%
                select(-.data$anom_value) %>%
                group_by(.data$var, .data$lon, .data$lat) %>%
                mutate(value = scale(.data$value)) %>%
                ungroup() %>%
                unite("expanded_grid",
                .data$lon:.data$lat, sep = ",", remove = T) %>%
                unite("expanded_grid", .data$expanded_grid, .data$var, sep = "_",
                      remove = T) %>%
                spread(.data$expanded_grid, .data$value) %>%
                select(-1) %>%
                princomp(scores = T)
        } else if (norm == F) {
            pca <- x %>%
                select(-.data$anom_value) %>%
                unite("expanded_grid", .data$lon, .data$lat, sep = ",", remove = T) %>%
                unite("expanded_grid", .data$expanded_grid, .data$var, sep = "_",
                      remove = T) %>%
                spread(.data$expanded_grid, .data$value) %>%
                select(-1) %>%
                princomp(scores = T)
        }
        rawLoadings <- pca$loadings[, 1:ncomp] %*% diag(pca$sdev, ncomp, ncomp)
        rotatedLoadings <- varimax(rawLoadings)$loadings
        scores <- scale(pca$scores[, 1:ncomp]) %*%
            varimax(rawLoadings)$rotmat %>%
            as_tibble() %>%
            setNames(paste0("PC", 1:ncomp))

        # Extreme scores calculation (Esteban et al., 2005)
        list_extreme_scores <- list()

        for (ii in 1:ncomp) {
            tt <- seq(1:ncomp)
            # Removeing extreme positive scores for all B4PCs except PC1, and so on
            subdata <- scores[apply(scores[, c(tt[-ii])] < extreme_scores,
                                    1, all), ]
            # Removeing extreme negative scores for all B4PCs except PC1, and so on
            subdata2 <- subdata[apply(subdata[, c(tt[-ii])] > -extreme_scores,
                                      1, all), ]

            # Ara promitgem dies extrems positius del PC1 A l'eliminar extrems
            # dels altres PCs, reflexem perfectament les coordenades
            # del PC1 +

            subdata_pos <- colMeans(subdata2[apply(subdata2[ii] > extreme_scores,
                                                   1, all), ])

            subdata_neg <- colMeans(subdata2[apply(subdata2[ii] < -extreme_scores,
                                                   1, all), ])
            result <- rbind(subdata_pos, subdata_neg)
            rownames(result) <- c(paste0("pc", ii, "+"), paste0("pc", ii, "-"))
            list_extreme_scores[[ii]] <- result
        }

        # Scores coordinates
        coordinates_scores <- do.call(rbind.data.frame, list_extreme_scores)
        coordinates_scores <- na.omit(coordinates_scores)

        # kmeans classification
        clas <- suppressWarnings(kmeans(scores,
                                        centers = coordinates_scores,
                                        iter.max = 1))$cluster %>%
            cbind.data.frame(time = unique(x$time)) %>%
            rename(WT = 1) %>%
            select(.data$time, .data$WT) %>%
            as_tibble()

        df_classified <- x %>%
            inner_join(clas, by = "time")

        df_classified_panels <- df_classified %>%
            group_by(.data$lon, .data$lat, .data$WT, .data$var) %>%
            mutate(mean_WT_value = mean(.data$value),
                   mean_WT_anom_value = mean(.data$anom_value),
                   ttest_anom = t.test(.data$anom_value, mu = 0)$p.value,
                   cv_WT_value = (sd(.data$value) / mean(.data$value)) * 100) %>%
            select(-.data$value, -.data$anom_value) %>%
            ungroup() %>%
            distinct(.data$lon, .data$lat, .data$WT, .data$var, .keep_all = T)

        return(list(clas = clas, grid_clas = df_classified_panels))

    } else if (matrix_mode == "T-mode") {
        if (norm == T) {
            pca <- x %>%
                select(-.data$anom_value) %>%
                group_by(.data$var, .data$lon, .data$lat) %>%
                mutate(value = scale(.data$value)) %>%
                ungroup() %>%
                spread(.data$time,.data$value) %>%
                select(-1:-3) %>%
                princomp(scores = T)

        } else if (norm == F) {
            pca <- x %>%
                select(-.data$anom_value) %>%
                spread(.data$time, .data$value) %>%
                select(-1:-3) %>%
                princomp(scores = T)
        }

        rawLoadings <- pca$loadings[, 1:ncomp] %*% diag(pca$sdev, ncomp, ncomp)
        rotatedLoadings <- varimax(rawLoadings)$loadings[]
        scores <- scale(pca$scores[, 1:ncomp]) %*% varimax(rawLoadings)$rotmat %>%
            as_tibble() %>%
            setNames(paste0("PC", 1:ncomp))


        ## Max Positive Loading WT procedure
        max_rotatedLoadings <- rotatedLoadings %>%
            as_tibble() %>%
            setNames(paste0("PC", 1:ncomp))
        max_rotatedLoadings$WT_max <- names(max_rotatedLoadings)[max.col(max_rotatedLoadings,
                                                                         ties.method = "first")]
        WT_max <- suppressWarnings(as.numeric(gsub(substr(max_rotatedLoadings$WT_max, 1, 2), "",
                                  max_rotatedLoadings$WT_max)))  # convertimos a numerico
        max_rotatedLoadings$WT_max <- WT_max

        
        ## Negative/postive WT procedure
        
        abs_rotatedLoadings <- abs(rotatedLoadings) %>%
          as_tibble() %>%
          setNames(paste0("PC", 1:ncomp))
        abs_rotatedLoadings$WT_abs <- names(abs_rotatedLoadings)[max.col(abs_rotatedLoadings,
                                                                         ties.method = "first")]
        WT_abs <- suppressWarnings(as.numeric(gsub(substr(abs_rotatedLoadings$WT_abs, 1, 2), "",
                                                   abs_rotatedLoadings$WT_abs)))  # convertimos a numerico
        abs_rotatedLoadings$WT_abs <- WT_abs
        
        WT_posneg <- rotatedLoadings %>%
            as_tibble() %>%
            setNames(1:ncomp) %>%
            cbind.data.frame(time = unique(x$time),
                             WT_abs =abs_rotatedLoadings$WT_abs,
                             WT_max = max_rotatedLoadings$WT_max) %>%
            as_tibble()

        # creant serie unica maxims loadings (+ o -)
        WT_posneg_g <- gather(WT_posneg,
                              key = "PCs",
                              value = "loadings",
                              1:ncomp) %>%
            mutate(PCs = as.numeric(.data$PCs)) %>%
            mutate(loadings = ifelse(.data$WT_abs == .data$PCs, loadings, NA)) %>%
            filter(!is.na(.data$loadings)) %>%
            arrange(.data$time) %>%
            mutate(symbol = ifelse(loadings >= 0, "+", "-"),
                   WT_pn = paste0(.data$PCs, .data$symbol))

        ## Final classifications
        clas_abs <- select(WT_posneg_g, .data$time, .data$WT_abs) %>% rename(WT = .data$WT_abs)
        clas_pn <- select(WT_posneg_g, .data$time, .data$WT_pn) %>% rename(WT = .data$WT_pn)
        clas_max <- select(WT_posneg_g, .data$time, .data$WT_max) %>% rename(WT = .data$WT_max)

        # absolute correlation classification gridding construction
        df_tmode_abs <- x %>%
            inner_join(clas_abs, by = "time")

        df_tmode_abs_panels <- df_tmode_abs %>%
            group_by(.data$lon, .data$lat, .data$WT, .data$var) %>%
            mutate(mean_WT_value = mean(.data$value),
                   mean_WT_anom_value = mean(.data$anom_value),
                   ttest_anom = t.test(.data$anom_value, mu = 0)$p.value,
                   cv_WT_value = (sd(.data$value) / mean(.data$value)) * 100) %>%
            select(-.data$value, -.data$anom_value) %>%
            ungroup() %>%
            distinct(.data$lon, .data$lat, .data$WT, .data$var, .keep_all = T)
        
        # Maximum correlation classification gridding construction
        df_tmode_max <- x %>%
          inner_join(clas_max, by = "time")
        
        df_tmode_max_panels <- df_tmode_max %>%
          group_by(.data$lon, .data$lat, .data$WT, .data$var) %>%
          mutate(mean_WT_value = mean(.data$value),
                 mean_WT_anom_value = mean(.data$anom_value),
                 ttest_anom = t.test(.data$anom_value, mu = 0)$p.value,
                 cv_WT_value = (sd(.data$value) / mean(.data$value)) * 100) %>%
          select(-.data$value, -.data$anom_value) %>%
          ungroup() %>%
          distinct(.data$lon, .data$lat, .data$WT, .data$var, .keep_all = T)

        # Positive/negative classification gridding construction
        df_tmode_pn <- x %>%
            inner_join(clas_pn, by = "time")

        df_tmode_pn_panels <- df_tmode_pn %>%
            group_by(.data$lon, .data$lat, .data$WT, .data$var) %>%
            mutate(mean_WT_value = mean(.data$value),
                   mean_WT_anom_value = mean(.data$anom_value),
                   ttest_anom = t.test(.data$anom_value, mu = 0)$p.value,
                   cv_WT_value = (sd(.data$value) / mean(.data$value)) * 100) %>%
            select(-.data$value, -.data$anom_value) %>%
            ungroup() %>%
            distinct(.data$lon, .data$lat, .data$WT, .data$var, .keep_all = T)


        return(list(clas_abs = clas_abs,
                    clas_max = clas_max,
                    clas_pn = clas_pn,
                    grid_clas_abs = df_tmode_abs_panels,
                    grid_clas_max = df_tmode_max_panels,
                    grid_clas_pn = df_tmode_pn_panels))

    } else {
        stop("please, choose one between S-mode and T-mode matrix modes")
    }
}
