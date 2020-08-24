#' PCA decision
#'
#' \code{pca_decision} plots the explained variances against the number of the principal component. In addition, it returns all the information about the PCA performance.
#'
#' @param x data.frame. A data.frame with the following variables: \code{lon, lat, time, value, anom_value}. See \code{tidy_nc}.
#' @param ncomp integer. Number of principal components to show/retain
#' @param norm logical. Default \code{TRUE}. \code{norm = TRUE} is recommended for classify two ore more variables.
#' @param matrix_mode character. The mode of matrix to use. Choose between S-mode and T-mode
#'
#' @return a list with: \itemize{
#'    \item{A list with class \code{princomp} containing all the results of the PCA }
#'    \item{A data frame containing the main results of the \code{ncomp} selected (standard deviation, proportion of variance and cumulative variance).}
#'    \item{A \code{ggplot2} object to visualize the scree test}
#' }
#'
#' @note To perform the PCA the \code{x} must contain more rows than columns. In addition, \code{x} cannot contain \code{NA} values.
#'
#' @examples
#' # Load data (mslp or precp_grid)
#' data(mslp)
#' data(z500)
#' # Tidying our atmospheric variables (500 hPa geopotential height
#' # and mean sea level pressure) together.
#'
#' # Time subset between two dates
#' atm_data1 <- tidy_nc(x = list(mslp,z500))
#'
#' # Deciding on the number of PC to retain
#' info <- pca_decision(atm_data1)
#'
#'
#' @seealso
#' \code{\link{tidy_nc}}
#'
#' @importFrom tidyr unite spread gather
#' @importFrom stats kmeans na.omit princomp varimax
#' @importFrom stringr str_pad
#' @importFrom tibble as_tibble rownames_to_column
#'
#' @export

pca_decision <- function(x, ncomp = 30, norm = T, matrix_mode = "S-mode") {
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
                unite("expanded_grid", .data$lon:.data$lat, sep = ",", remove = T) %>%
                unite("expanded_grid", .data$expanded_grid, .data$var, sep = "_",
                      remove = T) %>%
                spread(.data$expanded_grid, .data$value) %>%
                select(-1) %>%
                princomp(scores = T)
        }
    } else if (matrix_mode == "T-mode") {
        if (norm == T) {
            pca <- x %>%
                select(-.data$anom_value) %>%
                group_by(.data$var, .data$lon, .data$lat) %>%
                mutate(value = scale(.data$value)) %>%
                ungroup() %>%
                spread(.data$time, .data$value) %>%
                select(-1:-3) %>%
                princomp(cor = T, scores = T)
        } else if (norm == F) {
            pca <- x %>%
                select(-.data$anom_value) %>%
                spread(.data$time, .data$value) %>%
                select(-1:-3) %>%
                princomp(cor = T, scores = T)
        }
    }
    # summary table
    summaryPCA <- as.data.frame(rbind(
        SD <- sqrt(pca$sdev^2),
        proportion <- pca$sdev^2 / sum(pca$sdev^2),
        cumulative <- cumsum(pca$sdev^2) / sum(pca$sdev^2)))
    summaryPCA <- summaryPCA[, 1:ncomp]  #30 first PCA

    x <- NULL
    for (i in 1:ncomp) {
        x[i] <- paste0(i)
    }
    colnames(summaryPCA) <- x
    rownames(summaryPCA) <- c("sdev", "prop.variance", "cum.variance")

    summary_data_plot <- summaryPCA %>%
        tibble::rownames_to_column(var = "metrics") %>%
        gather(key = "PC", value = "value", 1:ncomp + 1)

    summary_data_plot$PC <- str_pad(summary_data_plot$PC, 2, "left", "0")

    sump <- ggplot2::ggplot() +
        ggplot2::geom_col(data = filter(summary_data_plot, .data$metrics == "cum.variance"),
                          ggplot2::aes(x = .data$PC, y = .data$value), color = "black") +
        ggplot2::geom_line(data = filter(summary_data_plot, .data$metrics == "prop.variance"),
                           ggplot2::aes(x = .data$PC, y = .data$value, group = 1), lwd = 0.75) +
        ggplot2::geom_point(data = filter(summary_data_plot, .data$metrics == "prop.variance"),
                           ggplot2::aes(x = .data$PC, y = .data$value), color = "red") +
        ggplot2::ylab("Proportion of Explained variance") +
        ggplot2::xlab("PC")+
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::theme_bw()

    return(list(PCA = pca, summary = summaryPCA, screeplot = sump))
}
