\name{unscale}
\alias{unscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Invert the effect of the scale function
}
\description{
  This function can be used to un-scale a set of values. This unscaling
  is done with the scaling information "hidden" on a scaled data set
  that should also be provided. This information is stored as an
  attribute by the function \code{scale()} when applied to a data
  frame. 
}
\usage{
unscale(vals, norm.data, col.ids)
}

\arguments{
  \item{vals}{
    A numeric matrix with the values to un-scale
}
  \item{norm.data}{
 A numeric and scaled matrix. This should be an object to which the
  function \code{scale()} was applied.
}
  \item{col.ids}{
The columns of the \code{vals} matrix that are to be un-scaled (defaults
  to all of them).
}
}
\value{
  An object with the same dimension as the parameter \code{vals}
}
\references{
Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{scale}}
}
\examples{
data(algae)
normData <- scale(algae[,4:12])
t <- rpartXse(a1 ~ .,as.data.frame(normData[1:100,]))
normPs <- predict(t,as.data.frame(normData[101:nrow(normData),]))
ps <- unscale(normPs,normData)
\dontrun{
 plot(algae[101:nrow(algae),'a1'],ps)
}
}
\keyword{models}
