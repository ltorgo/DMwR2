\name{resp}
\alias{resp}

\title{
Obtain the target variable values of a prediction problem
}
\description{
  This function obtains the values in the column whose name is the
  target variable of a prediction problem described by a formula.
}
\usage{
resp(formula, data)
}

\arguments{
  \item{formula}{
A formula describing a prediction problem
}
  \item{data}{
The data frame containing the data of the prediction problem
}
}
\value{
A vector of values
}
\references{
Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{as.formula}}
}
\examples{
data(algae)
tgt <- resp(a1 ~ ., algae)
summary(tgt)
\dontrun{
hist(tgt,main='Alga a1')
}
}

\keyword{models}

