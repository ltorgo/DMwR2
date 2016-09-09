\name{outliers.ranking}
\alias{outliers.ranking}

\title{
  Obtain outlier rankings
}
\description{
  This function uses hierarchical clustering to obtain a ranking of
  outlierness for a  set of cases. The ranking is obtained on the basis
  of the path each case follows within the merging steps of a
  agglomerative hierarchical clustering method. See the references for
  further technical details on how these rankings are obtained.
}
\usage{
outliers.ranking(data, test.data = NULL, method = "sizeDiff",
                 method.pars = NULL,
                 clus = list(dist = "euclidean",alg = "hclust",
                             meth = "ward.D"),
                 power = 1, verb = F)
}

\arguments{
  \item{data}{
    The data set to be ranked according to outlyingness. This parameter
  can also be the distance matrix of your additional data set, in case
  you wish to calculate these distances "outside" of this function.
  }
  \item{test.data}{
    If a data set is provided in this argument, then the rankings are
    obtained for these cases and not for the cases provided in the
    argument \code{data}. The clustering process driving the obtention of
    the rankings is carried out on the union of the two sets of data
    (\code{data} and \code{test.data}), but the resulting outlier ranking
    factors are only for the observations belonging to this set. This
    parameter defaults to \code{NULL}.
  }
  \item{method}{
    The method used to obtain the outlier ranking factors (see the
    Details section). Defaults to \code{"sizeDiff"}.
  }
  \item{method.pars}{
    A list with the parameter values specific to the method selected for
    obtaining the outlier ranks (see the Details section).
  }
  \item{clus}{
    This is a list that provides several parameters of the clustering
  process that drives the calculation of the outlier raking factors. If
  the parameter \code{data} is not a distance function, then this list
  should contain a component named \code{dist} with a value that should
  be one of the possible values of the parameter \code{method} the the
  function \code{dist()} (see the help of this function for further
  details). The list should also contain a component named \code{alg}
  with the name of the clustering algorithm that should be
  used. Currently, valid names are either "hclust" (the default) or
  "diana". Finally, in case the clustering algorithm is "hclust" then
  the list should also contain a component named \code{meth} with the
  name of the agglomerative method to use in the hierarchical clustering
  algorithm. This should be a valid value of the parameter \code{method}
  of the function \code{hclust()} (check its help page for further
  details). 
  }
  \item{power}{
    Integer value. It allows to raise the distance matrix to some power
    with the goal of "amplifying" the distance values (defaults to 1).
  }
  \item{verb}{
    Boolean value that determines the level of verbosity of the function (default to FALSE).
  }
}
\details{
  This function produces outlier ranking factors for a set of cases. The
  methodology used for obtaining these factors is described in Section
  4.4.1.3 of the book Data Mining with R (Torgo, 2010) and more details
  can be obtained in Torgo (2007). The methodology is based on the
  simple idea of using the information provided by an agglomerative
  hierarchical clustering algorithm to infer the degree of outlyingness
  of the observations. The basic assumption is that outliers should
  offer "more resistance" to being clustered, i.e. being merged on large
  groups of observations.

  The function was written to be used with the outcome of the
  \code{hclust()} R function that implements several agglomerative
  clustering methods. Although in theory the methodology could be used with
  any other agglomerative hierarchical clustering algorithm, the fact is
  that the code of this implementation strongly depends on the data
  structures produced by the \code{hclust()} function. As such if you
  wish to change the function to be able to use other clustering
  algorithms you should ensure that the data structures it produces are
  compatible with the requirements of our function. Specifically, your
  clustering algorithm should produce a list with a component named
  \code{merge} that should be a matrix describing the merging steps of
  the clustering process (see the help page of the \code{hclust()}
  function for a full description of this data structure). This is the
  only data structure that is required by our function and that is used
  from the object returned by clustering algorithm. The \code{diana()} clustering algorithm
  also produces this type of information and thus can also be used with
  our function by providing the value "diana" on the component
  \code{alg} of the list forming the parameter \code{clus}.

  There are essentially two ways of using this function. The first
  consists in giving it a data set on the parameter \code{data} and the
  function will rank these observations according to their
  outlyingness. The other consists in specifying two sets of data. One
  is the set for which you want the outlyingness factors that should be given  on the
  parameter \code{test.data}. The second  set is provided on the
  \code{data} parameter and it is used  to increase
  the ammount of data used in the clustering process to improve the
  statistical reliability of the process.

  In the first way of using this function that was described above the
  user can either supply the data set or the respective distance matrix.
  If the data set is provided then the user should specify the type of
  distance metric it should be used to calculate the distances between
  the observations. This is done by including a distance calculation
  method in the "dist" component of the list provided in parameter
  \code{clus}. This method should be a valid value of the parameter
  \code{method} of the R function \code{dist()} (see its help for details).

  This function currently implements three different methods for obtaing
  outlier ranking factors from the clustering process. These are:
  "linear", "sigmoid" and "sizeDiff" (the default). Irrespectively, of
  this method the outlyingness factor of observation X is obtained by:
  OF_H(X) = max_i of_i(X), where i represents the different merging
  steps of the clustering process and it goes from 1 to N-1, where N is
  the size of the data set to be clustered. The three methods differ in
  the way they calculate of_i(X) for each merging step. In  the "linear"
  method of_i(X) = i / (N-1) * p(|g|), where g is the group to which X
  belongs at the merging step i (each merging step involves two groups),
  and |g| is the size of that group. The function p() is a penalization
  factor depending on the size of the group. The larger this size the
  smaller the value of p(), p(s) = I(s < thr) * ( 1 - (s-1) / (N-2)),
  where I() is an indicator function and thr is a threshold defined as
  perc*N. The user should set the value of perc by including a component
  named "sz.perc" in the list provided in the parameter
  \code{method.pars}. In the "sigmoid" method of_i(X) = exp( -2 * (i -
  (N-1))^2 / (N-1)^2) * p(|g|), where the p() function has the same
  meaning as in the "linear" method but this time is defined as p(s) =
  I(s < 2*thr) * ( 1 - exp( -4 * (s-2*thr)^2 / (2*thr)^2)). Again thr is
  perc*N and the user must set the value of perc by including a component
  named "sz.perc" in the list provided in the parameter
  \code{method.pars}. Finally, the method "sizeDiff" defines of_i(X) =
  max ( 0, ( |g_{y,i}| - |g_{x,i}| ) / ( |g_{y,i}| + |g_{x,i}| ) ),
  where g_{y,i} and g_{x,i} are the two groups involved in the merge at
  step i, and g_{x,i} is the group which X belongs to. Note that if X
  belongs to the larger of the two groups this will get X a value of
  of_i() equals to zero.
}
\value{
  The result of this function is a list with four components. Component
  \code{rank.outliers} contains a vector with as many positions as there
  are cases to rank, where position i of the vector contains the rank
  order of the observation i. Component \code{prob.outliers} is another
  vector with the same size this time containing the outlyingness factor
  (the value of OF_H(X) described in the Details section) of each
  observation. Component \code{h} contains the object returned by the
  clustering process. Finally, component \code{dist} contains the
  distance matrix used i nthe clustering process.
}
\references{
  Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

  Torgo, L. (2007) : \emph{Resource-bounded Fraud Detection}, 
in Progress in Artificial Intelligence, 13th Portuguese Conference on Artificial Intelligence, EPIA 2007, Neves et. al (eds.). LNAI, Springer.

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\examples{
## Some examples with algae frequencies in water samples
data(algae)

## Trying to obtain a reanking of the 200 samples
o <- outliers.ranking(algae)

## As you may have observed the function complained about some problem
## with the dist() function. The problem is that the algae data frame
## contains columns (the first 3) that are factors and the dist() function
## assumes all numeric data.
## We can solve the problem by calculating the distance matrix "outside"
## using the daisy() function that handles mixed-mode data, as show in
## the code below that requires the R package "cluster" to be available
## dm <- daisy(algae)
## o <- outliers.ranking(dm)

## Now let us check the outlier ranking factors ordered by decreasing
## score of outlyingness
o$prob.outliers[o$rank.outliers]

## Another example with detection of fraudulent transactions
data(sales)

## trying to obtain the outlier ranks for the set of transactions of a
## salesperson regarding one particular product, taking into
## consideration the overall existing transactions of that product
s <- sales[sales$Prod == 'p1',c(1,3:4)]  # transactions of product p1
tr <- na.omit(s[s$ID != 'v431',-1])      # all except salesperson v431
ts <- na.omit(s[s$ID == 'v431',-1])

o <- outliers.ranking(data=tr,test.data=ts,
         clus=list(dist='euclidean',alg='hclust',meth='average'))
# The outlyingness factor of the transactions of this salesperson
o$prob.outliers

}

\keyword{models}

