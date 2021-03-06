% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4.Query.Subset.R
\docType{data}
\name{PWS.Loc.Sub.Chicago}
\alias{PWS.Loc.Sub.Chicago}
\title{Chicago History Dataset}
\format{An object of class \code{PWS.Query.Subset} of length 1.}
\usage{
PWS.Loc.Sub.Chicago
}
\description{
This contains subregion locations for the Personal Weather Stations in
a 2km region centered near Chicago, Illinois from March 13, 2016
called with PWS.Loc.Sub.Chicago <- PWS.Query.Subset(PWS.Loc.Chicago, -87.62, 41.88, 2)
where the PWS.Loc.Chicago call is described in the PWS.Locations S4 Class (which is a 5km region).
The subregion lies within the larger region.
}
\examples{
data(PWS.Loc.Sub.Chicago.rda)
}
\author{
wundr team
}
\keyword{datasets}

