\name{nrLinesFile}
\alias{nrLinesFile}

\title{
Counts the number of lines of a file
}
\description{
  Function for counting the number of lines of very large text
  files. Note that it only works on unix-based systems as it uses the
  \code{wc} command line utility
}
\usage{
nrLinesFile(f)
}

\arguments{
  \item{f}{
    A file name (a string)
  }
}
\details{
  The function creates a data set corresponding to the embed of a
  certain size of a given univariate time series.

  For instance for an embed of size 3 each row of the data set will
  contain the value of the series at time t, t-1 and t-2.

}
\value{
  An integer

}
\references{
Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{sampleCSV}}
}
\keyword{models}