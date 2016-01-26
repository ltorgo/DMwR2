\name{centralImputation}
\alias{centralImputation}

\title{
Fill in NA values with central statistics
}
\description{
  This function fills in any NA value in all columns of a data frame
  with the statistic of centrality (given by the function
  \code{centralvalue()}) of the respective column.

}
\usage{
centralImputation(data)
}

\arguments{
  \item{data}{
The data frame
}
}
\value{
  A new data frame with no NA values
}
\references{
Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{knnImputation}}, \code{\link{centralValue}}, \code{\link{complete.cases}}, \code{\link{na.omit}}
}
\examples{
data(algae)
cleanAlgae <- centralImputation(algae)
summary(cleanAlgae)
}
\keyword{models}
