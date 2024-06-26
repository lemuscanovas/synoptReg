% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lamb_clas.R
\name{lamb_clas}
\alias{lamb_clas}
\title{Objective Lamb Weather Type Classification}
\usage{
lamb_clas(points, msl, U = FALSE, thr = c(6, 6))
}
\arguments{
\item{points}{16 point pair of coordinates obtained from \code{get_lamb_points}.}

\item{msl}{Mean Sea Level pressure gridded data.}

\item{U}{Logical. If T, Jones et al. 2013 approach is applied, maintaining the U-type in the classification. If F, U is removed as detailed in Trigo and DaCamara, 2000.}

\item{thr}{threshold used for Unclassified days (total shear vorticity and total flow, respectively). Default c(6,6).}
}
\value{
A list with: \itemize{
   \item{A data.frame containing the dates and the weather types.}
   \item{A data frame containing the gridded data grouped by circulation types.}
}
}
\description{
Calculates the classification of the main weather types
             for the 16-points defined in \code{get_lamb_points}. Wind-flow characteristics
             are computed for the daily pressure field according to the rules proposed by the original
             Jenkinson and Collison classification (see Jenkinson and Collison, 1977; Jones et al., 2013) (1), and 
             to the rules proposed by Trigo and DaCamara, 2000 (2).
}
\examples{
data(msl)
msl$value <- msl$value/100
points <- get_lamb_points(x = 5,y = 40)
lamb_clas(points = points, msl = msl)

}
\references{
{
Jenkinson, A.F., Collison F.P (1977)
\emph{An initial climatology of gales over the North Sea}
Synoptic Climatology Branch Memorandum, No. 62.Meteorological Office: Bracknell, England.

Jones, P. D., Hulme M., Briffa K. R. (1993)
\emph{A comparison of Lamb circulation types with an objective classification scheme}
Int. J. Climatol. 13: 655–663.

Jones, P. D., Harpham C, Briffa K. R. (2013)
\emph{Lamb weather types derived from Reanalysis products}
Int. J. Climatol. 33: 1129–1139.
}

Trigo, R., DaCamara C. (2000)
\emph{Circulation weather types and their impact on the precipitation regime in Portugal}
Int. J. Climatol. 20: 1559-1581.
}
\seealso{
\code{\link{get_lamb_points}}
}
