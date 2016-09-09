\name{dist.to.knn}
\alias{dist.to.knn}

\title{
  An auxiliary function of \code{lofactor()}
}
\description{
  This function returns an object in which columns contain the
  indices of the first k neighbors followed by the distances to each
  of these neighbors.
}
\usage{
dist.to.knn(dataset, neighbors)
}

\arguments{
  \item{dataset}{
    A data set that will be internally coerced into a matrix.
}
  \item{neighbors}{
    The number of neighbours.
}
}
\details{
  This function is strongly based on the code provided by Acuna
  et. al. (2009) for the previously available \code{dprep} package.
}
\value{
  A matrix
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

