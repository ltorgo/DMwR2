\name{createEmbedDS}
\alias{createEmbedDS}

\title{
Creates an embeded data set from an univariate time series
}
\description{
  Function for creating and embeded data set from a univariate time
  series given an embed size
}
\usage{
createEmbedDS(s, emb=4)
}

\arguments{
  \item{s}{
    A univariate time series (can be a numeric vector or a xts object)
  }
  \item{emb}{
    The size of the embed for creating the data set (defaults to 4)
  }
}
\details{
  The function creates a data set corresponding to the embed of a
  certain size of a given univariate time series.

  For instance for an embed of size 3 each row of the data set will
  contain the value of the series at time t, t-1 and t-2.

}
\value{
  Either a matrix or a multivariate xts, depending on whether the input
  series is a numberic vector or a univariate xts, respectively.
}
\references{
Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{embed}}
}
\examples{
## A simple example with a random time series
x <- rnorm(100)
head(x)
dataSet <- createEmbedDS(x,emb=5)
head(dataSet)
}
\keyword{models}
