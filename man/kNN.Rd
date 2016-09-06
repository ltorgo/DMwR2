\name{kNN}
\alias{kNN}

\title{
k-Nearest Neighbour Classification
}
\description{
  This function provides a formula interface to the existing
  \code{knn()} function of package \code{class}. On top of this type of
  convinient interface, the function also allows standardization of the
  given data.
}
\usage{
kNN(form, train, test, stand = TRUE, stand.stats = NULL, ...)
}

\arguments{
  \item{form}{
    An object of the class \code{formula} describing the functional form
    of the classification model.
}
\item{train}{
  The data to be used as training set.
}
\item{test}{
  The data set for which we want to obtain the k-NN classification,
  i.e. the test set.
}
\item{stand}{
  A boolean indicating whether the training data should be previously
  normalized before obtaining the k-NN predictions (defaults to TRUE).
}
\item{stand.stats}{
  This argument allows the user to supply the centrality and spread
  statistics that will drive the standardization. If not supplied they
  will default to the statistics used in the function \code{scale()}. If
  supplied they should be a list with two components, each beig a vector
  with as many positions as there are columns in the data set. The
  first vector should contain the centrality statistics for each column,
  while the second vector should contain the spread statistc values.
}
  \item{\dots}{
    Any other parameters that will be forward to the \code{knn()}
    function of package \code{class}.
}
}
\details{
  This function is essentially a convenience function that provides a
  formula-based interface to the already existing \code{knn()}
  function of package \code{class}. On top of this type of interface it
  also incorporates some facilities in terms of standardization of the
  data before the k-nearest neighbour classification algorithm is
  applied. This algorithm is based on the distances between
  observations, which are known to be very sensitive to different scales
  of the variables and thus the usefulness of standardization.
}
\value{
  The return value is the same as in the \code{knn()}
  function of package \code{class}. This is a factor of classifications
  of the test set cases. 
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\link{knn}}, \code{\link{knn1}}, \code{\link{knn.cv}}
}
\examples{
## A small example with the IRIS data set
data(iris)

## Split in train + test set
idxs <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
trainIris <- iris[idxs,]
testIris <- iris[-idxs,]

## A 3-nearest neighbours model with no standardization
nn3 <- kNN(Species ~ .,trainIris,testIris,stand=FALSE,k=3)

## The resulting confusion matrix
table(testIris[,'Species'],nn3)

## Now a 5-nearest neighbours model with standardization
nn5 <- kNN(Species ~ .,trainIris,testIris,stand=TRUE,k=5)

## The resulting confusion matrix
table(testIris[,'Species'],nn5)


}

\keyword{models}

