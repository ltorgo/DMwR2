\name{manyNAs}
\alias{manyNAs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find rows with too many NA values
}
\description{
Small utility function to obtain the number of the rows
in a data frame that have  a "large" number of 
unknown values.
"Large" can be defined either as a proportion of the
number of columns or as the number in itself.
}
\usage{
manyNAs(data, nORp = 0.2)
}

\arguments{
  \item{data}{
    A data frame with the data set.
}
  \item{nORp}{
A number controlling when a row is considered to have too many NA values
  (defaults to 0.2, i.e. 20\% of the columns). If no rows satisfy the
  constraint indicated by the user, a warning is generated.
}
}
\value{
A vector with the IDs of the rows with too many NA values. If there are
  no rows with many NA values and error is generated.
}
\references{
Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{complete.cases}}, \code{\link{na.omit}}
}
\examples{
data(algae)
manyNAs(algae)
}
\keyword{models}

