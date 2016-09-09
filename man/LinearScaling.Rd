\name{LinearScaling}
\alias{LinearScaling}

\title{
Normalize a set of continuous values using a linear scaling
}
\description{
  Function for normalizing the range of values of a continuous variable
  using  a linear scaling within the range of the variable.

}
\usage{
LinearScaling(x, mx = max(x, na.rm = T), mn = min(x, na.rm = T))
}

\arguments{
  \item{x}{
A vector with numeric values

}
  \item{mx}{
The maximum value of the continuous variable being normalized
  (defaults to the maximum of the values in \code{x}).

}
  \item{mn}{
The minimum value of the continuous variable being normalized
  (defaults to the minimum of the values in \code{x}).

}
}
\details{
  The linear scaling normalization consist in transforming the value x into

  (x - MIN) / (MAX - MIN)

}
\value{
  An object with the same dimensions as \code{x} but with the values normalized
}
\references{
Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{scale}}, \code{\link{SoftMax}}, \code{\link{ReScaling}}
}
\examples{
## A simple example with the algae data set
data(iris)
summary(LinearScaling(iris[,'Sepal.Length']))
summary(iris[,'Sepal.Length'])
}
\keyword{models}