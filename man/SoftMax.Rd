\name{SoftMax}
\alias{SoftMax}

\title{
Normalize a set of continuous values using SoftMax
}
\description{
  Function for normalizing the range of values of a continuous variable
  using the SoftMax function (Pyle, 199).
}
\usage{
SoftMax(x, lambda = 2, avg = mean(x, na.rm = T), std = sd(x, na.rm = T))
}

\arguments{
  \item{x}{
A vector with numeric values
}
  \item{lambda}{
A numeric value entering the formula of the soft max function (see
  Details). Defaults to 2.
}
  \item{avg}{
The statistic of centrality of the continuous variable being normalized
  (defaults to the mean of the values in \code{x}).
}
  \item{std}{
The statistic of spread of the continuous variable being normalized
  (defaults to the standard deviation of the values in \code{x}).

}
}
\details{
  The Soft Max normalization consist in transforming the value x into

  1 / [ 1+ exp( (x-AVG(x))/(LAMBDA*SD(X)/2*PI) ) ]

}
\value{
  An object with the same dimensions as \code{x} but with the values normalized
}
\references{
Pyle, D. (1999). \emph{Data preparation for data mining}. Morgan Kaufmann.

Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{scale}}, \code{\link{LinearScaling}}, \code{\link{ReScaling}}
}
\examples{
## A simple example with the algae data set
summary(SoftMax(algae[,'NO3']))
summary(algae[,'NO3'])
}
\keyword{models}