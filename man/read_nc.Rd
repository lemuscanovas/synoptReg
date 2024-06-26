% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_nc.R
\name{read_nc}
\alias{read_nc}
\title{Read and tidy your NetCDF}
\usage{
read_nc(
  x,
  level = NULL,
  anomaly = F,
  time_subset = NULL,
  month_subset = NULL,
  crop_area = NULL,
  aggregate = NULL
)
}
\arguments{
\item{x}{path to file or SpatRaster object.}

\item{level}{integer. Default NULL. Select a specific pressure level if the data contains multiple levels.}

\item{anomaly}{logical. If TRUE it convets into anomalies, based on their corresponding monthly means.}

\item{time_subset}{character or Date. Default NULL. Provide a vector of dates to subset the original SpatRaster time series.}

\item{month_subset}{integer. Default NULL. Provide a vector of integers to subset the original SpatRaster by months.}

\item{crop_area}{integer. Default NULL. Provide a vector of coordinates (xmin,xmax,ymin,ymax) to crop the original SpatRaster domain.}

\item{aggregate}{integer. Default NULL. Resampling to a coarser resolution. Useful to save memory when processing heavy time-consuming datasets.}
}
\value{
A SpatRaster object. It must be converted to daily if the input is hourly.
}
\description{
Read and tidy your initial NetCDF by turning the absolute values into monthly-based anomalies, and by subsetting the time series and geogprahical area of your atmospheric variable.
}
\examples{
# Load data (mslp or precp_grid)
slp_file <- system.file("extdata", "mslp_ei.nc", package = "synoptReg")

# Reading data simply
slp <- read_nc(slp_file)

# Converting to monthly based anomalies and just for October, November and December
slp <- read_nc(slp_file, anomaly = TRUE, month_subset = 10:12)

}
\seealso{
\code{\link{read_nc}}
}
