\name{rpartXse}
\alias{rpartXse}

\title{
Obtain a tree-based model
}
\description{
  This function is based on the tree-based framework provided by the
  \code{rpart} package (Therneau et. al. 2010). It basically, integrates
  the tree growth and tree post-pruning in a single function call. The
  post-pruning phase is essentially the 1-SE rule described in the CART
  book (Breiman et. al. 1984). 
}
\usage{
rpartXse(form, data, se = 1, cp = 0, minsplit = 6, verbose = F, ...)
}

\arguments{
  \item{form}{
A formula describing the prediction problem
}
\item{data}{
  A data frame containg the training data to be used to obtain the
  tree-based model
}
\item{se}{
  A value with the number of standard errors to use in the post-pruning
  of the tree using the SE rule (defaults to 1)
}
\item{cp}{
  A value that controls the stopping criteria used to stop the initial
  tree growth (defaults to 0)
}
  \item{minsplit}{
  A value that controls the stopping criteria used to stop the initial
  tree growth (defaults to 6)
}
  \item{verbose}{
The level of verbosity of the function (defaults to F)
}
  \item{\dots}{
Any other arguments that are passed to the \code{rpart()} function
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
  Therneau, T. M. and Atkinson, B.; port by Brian
  Ripley. (2010). \emph{rpart: Recursive Partitioning}. R package version
  3.1-46.
  
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

