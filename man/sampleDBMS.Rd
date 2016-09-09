\name{sampleDBMS}
\alias{sampleDBMS}

\title{
Drawing a random sample of records of a table stored in a DBMS
}
\description{
  Function for obtaining a random sample of records from a very large
  table stored in a databased managment system, whitout having to load
  in the full table into memory. Targets  situations where the full data
  does not fit in the computer memory so 
  usage of the standard \code{sample} function is not possible.
}
\usage{
sampleDBMS(dbConn, tbl, percORn, mxPerc=0.5)
}

\arguments{
  \item{dbConn}{
    A data based connection object from the \code{DBI} package, that
  contains the result of establishing the connection to your target
  database in the respective database managment system.
  }
  \item{tbl}{
    A string containing the name of the (large) table in the database from which
  you want draw a random sample of records.
  }
  \item{percORn}{
    Either the percentage of number of rows of the file or the actual
  number of rows, the sample should have
  }
  \item{mxPerc}{
    A maximum threshold for the percentage the sample is allowed to have
  (defaults to 0.5)
  }
}

\details{
  This function can be used to draw a random sample of records from a very
  large table of a database managment system. This is particularly
  usefull when you can not afford 
  to load the full table into memory to use R functions like \code{sample} to
  obtain the sample.

  The function obtains the sample of rows without actually loading the
  full data into memory - only the final sample is loaded into main
  memory.

  The function assumes you have alread established and opened a
  connection to the database and receives as argument the DBI connection
  object. 

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
\code{\link{sampleCSV}}, \code{\link{sample}}
}
\examples{
## A simple example over a table on a MySQL database
\dontrun{
library(DBI)
library(RMySQL)
drv <- dbDriver("MySQL")  # Loading the MySQL driver
con <- dbConnect(drv,dbname="myDB",  
                 username="myUSER",password="myPASS",
                 host="localhost")
d <- sampleDBMS(con,"largeTable",10000)
}
}
\keyword{models}