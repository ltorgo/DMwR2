\name{reachability}
\alias{reachability}

\title{
  An auxiliary function of \code{lofactor()}
}
\description{
    This function computes the reachability measure for each instance
     of a dataset. This result is used later to compute the Local
     Outlyingness Factor.
}
\usage{
reachability(distdata, k)
}

\arguments{
  \item{distdata}{
The matrix of distances.
}
  \item{k}{
The  number of neighbors.
}
}
\details{
  This function is strongly based on the code provided by Acuna
  et. al. (2009) for the previously available \code{dprep} package.
}
\value{
  A vector.
}
\references{
 Acuna, E., and Members of the CASTLE group at UPR-Mayaguez, (2009).
   \emph{dprep: Data preprocessing and visualization functions for classification}. R package version 2.1.

Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  
 }
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{lofactor}}
}
\keyword{ models }
