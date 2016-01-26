\name{ReScaling}
\alias{ReScaling}

\title{
Re-scales a set of continuous values into a new range using a linear scaling
}
\description{
  Function for normalizing the range of values of a continuous variable
  into a new interval using  a linear scaling.
}
\usage{
ReScaling(x, t.mn, t.mx, d.mn = min(x,na.rm=T), d.mx = max(x,na.rm=T))
}

\arguments{
  \item{x}{
A vector with numeric values

}
  \item{t.mn}{
The minimum value in the new scale
}
  \item{t.mx}{
The maximum value in the new scale
}
  \item{d.mn}{
The minimum value of the continuous variable being normalized
  (defaults to the minimum of the values in \code{x}).

}
  \item{d.mx}{
The maximum value of the continuous variable being normalized
  (defaults to the maximum of the values in \code{x}).

}
}
\details{
  The re-scaling consist in transforming the value x into

  sc*x + t.mn - sc*d.mn

  where sc =  (t.mx-t.mn)/(d.mx-d.mn)

}
\value{
  An object with the same dimensions as \code{x} but with the values normalized
}
\references{
Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{scale}}, \code{\link{SoftMax}}, \code{\link{LinearScaling}}
}
\examples{
## A simple example with the algae data set
summary(LinearScaling(algae[,'NO3']))
summary(ReScaling(LinearScaling(algae[,'NO3']),-10,10))
}
\keyword{models}