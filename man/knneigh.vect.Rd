\name{knneigh.vect}
\alias{knneigh.vect}

\title{
  An auxiliary function of \code{lofactor()}
}
\description{
Function that returns the distance from a vector \code{x} to    its
     k-nearest-neighbors in the matrix \code{data}
}
\usage{
knneigh.vect(x, data, k)
}

\arguments{
  \item{x}{
An observation.
}
  \item{data}{
A data set that will be internally coerced into a matrix.
}
  \item{k}{
The number of neighbours.
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

  Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{lofactor}}
}
\keyword{ models }
