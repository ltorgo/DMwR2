\name{sampleCSV}
\alias{sampleCSV}

\title{
Drawing a random sample of lines from a CSV file
}
\description{
  Function for obtaining a random sample of lines from a very large CSV
  file, whitout having to load in the full data into memory. Targets
  situations where the full data does not fit in the computer memory so
  usage of the standard \code{sample} function is not possible.
}
\usage{
sampleCSV(file, percORn, nrLines, header=TRUE, mxPerc=0.5)
}

\arguments{
  \item{file}{
    A file name (a string)
  }
  \item{percORn}{
    Either the percentage of number of rows of the file or the actual
  number of rows, the sample should have
  }
  \item{nrLines}{
    Optionally you may indicate the number of rows of the file if you
  know it before-hand, otherwise the function will count them for you
  }
  \item{header}{
    Whether the file has a header line or not (a Boolean value)
  }
  \item{mxPerc}{
    A maximum threshold for the percentage the sample is allowed to have
  (defaults to 0.5)
  }
}

\details{
  This function can be used to draw a random sample of lines from a very
  large CSV file. This is particularly usefull when you can not afford
  to load the file into memory to use R functions like \code{sample} to
  obtain the sample.

  The function obtains the sample of rows without actually loading the
  full data into memory - only the final sample is loaded into main
  memory.

  The function is based on unix-based utility programs (\code{perl} and \code{wc}) so
  it is limited to this type of platforms. The function will not run on
  other platforms (it will check the system variable \code{.Platform$OS.type}), although you may wish to check the function code and
  see if you can adapt it to your platform.

}
\value{
  A data frame

}
\references{
Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{nrLinesFile}}, \code{\link{sample}}, \code{\link{sampleDBMS}}
}
\keyword{models}