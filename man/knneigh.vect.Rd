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
