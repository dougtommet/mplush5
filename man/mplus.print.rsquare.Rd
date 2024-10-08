% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mplus_print.R
\name{mplus.print.rsquare}
\alias{mplus.print.rsquare}
\title{Mplus Print R Square}
\usage{
mplus.print.rsquare(file, invariance = 0, cluster = FALSE)
}
\arguments{
\item{file}{the quoted name of an existing H5 file}

\item{invariance}{0 (not invariance model, default)}

\item{cluster}{logical}
}
\value{
A data frame containing the R-square is printed
}
\description{
Show the R-square
}
\examples{

ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
r2 <- mplus.print.rsquare(ex3_1)
r2

}
