\name{SelfTrain}
\alias{SelfTrain}

\title{
  Self train a model on semi-supervised data
}
\description{
  This function can be used to learn a classification model from
  semi-supervised data. This type of data includes observations for
  which the class label is known as well as observation with unknown
  class. The function implements a strategy known as self-training to be able
  to cope with this semi-supervised learning problem. The function can
  be applied to any classification algorithm that is able to obtain
  class probabilities when asked to classify a set of test cases (see
  the Details section).
}
\usage{
SelfTrain(form,data,
          learner, learner.pars=list(),
          pred, pred.pars=list(),
          thrConf=0.9,
          maxIts=10,percFull=1,
          verbose=FALSE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
}
\item{data}{
  A data frame containing the available training set that is supposed to
  contain some rows for which the value of the target variable is
  unknown (i.e. equal to \code{NA}).
}
  \item{learner}{
    An object of class \code{learner} (see \code{class?learner} for
  details), indicating the base classification algorithm to use in the
  self-training process.
}
  \item{learner.pars}{
    A \code{list} with parameters that are to be passed to the
  \code{learner} function at each self-training iteration.
}
  \item{pred}{
    A string with the name of a function that will carry out the
  probabilistic classification tasks that will be necessary during the
  self training process (see the Details section).
}
  \item{pred.pars}{
    A \code{list} with parameters that are to be passed to the
  \code{pred} function at each self-training iteration when obtaining
  the predictions of the models.
}
  \item{thrConf}{
    A number between 0 and 1, indicating the required classification
  confidence for an unlabelled case to be added to the labelled data set
  with the label predicted predicted by the classification algorithm.
}
  \item{maxIts}{
    The maximum number of iterations of the self-training process.
}
  \item{percFull}{
    A number between 0 and 1. If the percentage of labelled cases
  reaches this value the self-training process is stoped.
}
\item{verbose}{
  A boolean indicating the verbosity level of the function.
}
}
\details{
  Self-training (e.g. Yarowsky, 1995) is a well-known strategy to handle
  classification problems where a subset of the available training data
  has an unknown class label. The general idea is to use an iterative
  process where at each step we try to augment the set of labelled cases
  by "asking" the current classification model to label the unlabelled
  cases and choosing the ones for which the model is more confident on
  the assigned label to be added to the labeled set. With this extended
  set of labelled cases a new classification model is learned and the
  process is repeated until certain termination criteria are met.

  This implementation of the self-training algorithm is generic in the
  sense that it can be used with any baseline classification learner
  provided this model is able to produce confidence scores for its
  predictions. The user needs to take care of the learning of the models
  and of the classification of the unlabelled cases. This is done as
  follows. The user supplies a \code{learner} object (see
  \code{class?learner} for details) in parameter \code{learner} to represent the algorithm to be
  used to obtain the classification models on each iteration of the 
  self-training process. Furthermore, the user should create a function,
  whose named should be given in the parameter \code{predFunc}, that
  takes care of the classification of the currently unlabelled cases, on
  each iteration. This function should be written so that it receives as
  first argument the learned classification model (with the current
  training set), and a data frame with test cases in the second
  argument. This user-defined function should return a data frame with
  two columns and as many rows as there are rows in the given test
  set. The first column of this data frame should contain the assigned
  class labels by the provided classification model, for the respective
  test case. The second column should contain the confidence (a number
  between 0 and 1) associated to that classification. See the Examples
  section for an illustration of such user-defined function.

  This function implements the iterative process of self training. On
  each iteration the provided learner is called with the set of labelled
  cases within the given data set. Unlabelled cases should have the
  value \code{NA} on the column of the target variable.  The obtained
  classification model is then passed to the user-defined "predFunc"
  function together with the subset of the data that is unlabelled. As
  mentioned above this function returns a set of predicted class labels
  and the respective confidence. Test cases with confidence above the
  user-specified threshold (parameter \code{thrConf}) will be added to
  the labelled training set, with the label assigned by the current
  model. With this new training set a new classification model is
  obtained and the overall process repeated.

  The self-training process stops if either there are no classifications
  that reach the required confidence level, if the maximum number of
  iterations is reached, or if the size of the current labelled training
  set is alread the target percentage of the given data set.
}
\value{
This function returns a classification model. This will be an object of
  the same class as the object returned by the base classification learned
  provided by the user.
}
\references{
Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

  Yarowski, D. (1995). \emph{Unsupervised word sense disambiguation
  rivaling supervised methods}. In Proceedings of the 33rd Annual
  Meeting of the association for Computational Linguistics (ACL), pages 189-196.
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\examples{
## Small example with the Iris classification data set
data(iris)

## Dividing the data set into train and test sets
idx <- sample(150,100)
tr <- iris[idx,]
ts <- iris[-idx,]

## Learn a tree with the full train set and test it
stdTree <- rpartXse(Species~ .,tr,se=0.5)
table(predict(stdTree,ts,type='class'),ts$Species)

## Now let us create another training set with most of the target
## variable values unknown
trSelfT <- tr
nas <- sample(100,70)
trSelfT[nas,'Species'] <- NA

## Learn a tree using only the labelled cases and test it
baseTree <- rpartXse(Species~ .,trSelfT[-nas,],se=0.5)
table(predict(baseTree,ts,type='class'),ts$Species)

## The user-defined function that will be used in the self-training process
f <- function(m,d) { 
      l <- predict(m,d,type='class')
      c <- apply(predict(m,d),1,max)
      data.frame(cl=l,p=c)
}

## Self train the same model using the semi-superside data and test the
## resulting model
treeSelfT <- SelfTrain(Species~ .,trSelfT,'rpartXse',list(se=0.5),'f')
table(predict(treeSelfT,ts,type='class'),ts$Species)
}

\keyword{models}

