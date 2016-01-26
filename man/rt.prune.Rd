\name{rt.prune}
\alias{rt.prune}

\title{
Prune a tree-based model using the SE rule
}
\description{
  This function implements the SE post pruning rule described in the
  CART book (Breiman et. al., 1984)
}
\usage{
rt.prune(tree, se = 1, verbose = T, ...)
}

\arguments{
  \item{tree}{
    An \code{rpart} object
}
  \item{se}{
The value of the SE threshold (defaulting to 1)
}
\item{verbose}{
  The level of verbosity (defaulting to T)
}
  \item{\dots}{
Any other arguments passed to the function \code{prune.rpart()}
}
}
\details{
  The x-SE rule for tree post-pruning is based on the 
  cross-validation estimates of the error of the sub-trees of the
  initially grown tree, together with the
  standard errors of these estimates. These values are used to select
  the final tree model. Namely, the selected tree is the smallest tree
  with estimated error less than the B+x*SE, where B is the lowest
  estimate of error and SE is the standard error of this B estimate.
}
\value{
  A \code{rpart} object
}
\references{
Breiman, L., Friedman, J., Olshen, R., and Stone,
C. (1984). \emph{Classification and regression trees}. Statistics/Probability Series. Wadsworth & Brooks/Cole
Advanced Books & Software.

Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{rt.prune}}, \code{\link{rpart}}, \code{\link{prune.rpart}}
}
\examples{
data(iris)
tree <- rpartXse(Species ~ ., iris)
tree

## A visual representation of the classification tree
\dontrun{
prettyTree(tree)
}
}

\keyword{models}
