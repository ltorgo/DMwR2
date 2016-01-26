\name{lofactor}
\alias{lofactor}

\title{
  An implementation of the LOF algorithm
}
\description{
This function obtain local outlier factors using the LOF
  algorithm. Namely, given a data set it produces a vector of local
  outlier factors for each case.
}
\usage{
lofactor(data, k)
}

\arguments{
  \item{data}{
    A data set that will be internally coerced into a matrix.
}
  \item{k}{
    The number of neighbours that will be used in the calculation of the
  local outlier factors.
}
}
\details{
  This function re-implements the code previously made available in the
  \code{dprep} package (Acuna et. al., 2009) that was removed from
  CRAN. This code in turn is an implementation of the LOF method by
  Breunig et. al. (2000). See this reference to understand the full
  details on how these local outlier factors are calculated for each
  case in a data set.
}
\value{
  The function returns a vector of local outlier factors (numbers). This
  vector has as many values as there are rows in the original data set.
}
\references{
 Acuna, E., and Members of the CASTLE group at UPR-Mayaguez, (2009).
   \emph{dprep: Data preprocessing and visualization functions for classification}. R package version 2.1.

 Breunig, M., Kriegel, H., Ng, R., and Sander, J. (2000). \emph{LOF: identifying
   density-based local outliers}. In ACM Int. Conf. on Management of Data,
   pages 93-104.

  Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\examples{
data(iris)
lof.scores <- lofactor(iris[,-5],10)
}

\keyword{models}

