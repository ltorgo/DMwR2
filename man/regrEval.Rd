\name{regrEval}
\alias{regrEval}

\title{
Calculate Some Standard Regression Evaluation Statistics
}
\description{
This function is able to calculate a series of regression evaluation
statistics given two vectors: one with the true target variable values,
and the other with the predicted target variable values.
}
\usage{
regrEval(trues, preds,
          stats = if (is.null(train.y)) c("mae","mse","rmse","mape")
                  else c("mae","mse","rmse","mape","nmse","nmae"),
          train.y = NULL)
}

\arguments{
  \item{trues}{
A numeric vector with the true values of the target variable.
}
  \item{preds}{
A numeric vector with the predicted values of the target variable.
}
  \item{stats}{
A vector with the names of the evaluation statistics to
  calculate. Possible values are "mae", "mse", "rmse", "mape", "nmse" or
  "nmae". The two latter require that the parameter \code{train.y}
  contains a numeric vector of target variable values (see below). 
}
\item{train.y}{
  In case the set of statistics to calculate include either "nmse" or
  "nmae", this parameter should contain a numeric vector with the values
  of the target variable on the set of data used to obtain the model
  whose performance is being tested.
}
}
\details{
The regression evaluation statistics calculated by this function belong
  to two different groups of measures: absolute and relative. The former
  include "mae", "mse", and "rmse" and are calculated as follows:

  "mae": mean absolute error, which is calculated as sum(|t_i - p_i|)/N,
  where t's are the true values and p's are the predictions, while N is
  supposed to be the size of both vectors.

  "mse": mean squared error, which is calculated as sum( (t_i - p_i)^2
  )/N

  "rmse": root mean squared error that is calculated as sqrt(mse)

  The remaining measures ("mape", "nmse" and "nmae") are relative
  measures, the two later
  comparing the performance of the model with a baseline. They are
  unit-less measures with values always greater than 0. In the case of
  "nmse" and "nmae" the values are expected to be in the interval [0,1]
  though occasionaly scores can overcome 1, which means that your model
  is performing worse than the baseline model. The baseline used in our
  implementation is a constant model that always predicts the average
  target variable value, estimated using the values of this variable on
  the training data (data used to obtain the model that generated the
  predictions), which should be 
  given in the parameter \code{train.y}. The relative error measure
  "mape" does not require a baseline. It simply calculates the average
  percentage difference between the true values and the
  predictions.

  These measures are calculated as follows:

  "mape": sum(|(t_i - p_i) / t_i|)/N

  "nmse": sum( (t_i - p_i)^2 ) / sum( (t_i - AVG(Y))^2 ), where AVG(Y)
  is the average of the values provided in vector \code{train.y}

  "nmae": sum(|t_i - p_i|) / sum(|t_i - AVG(Y)|)

}
\value{
A named vector with the calculated statistics.
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\note{
In case you require either "nmse" or "nmae" to be calculated you must
  supply a vector of numeric values through the parameter
  \code{train.y}, otherwise the function will return an error
  message. The average of these values will be used as the baseline
  model against which your model predictions will be compared to.
}
\seealso{
 \code{\link{class.eval}}
}
\examples{
## Calculating several statistics of a regression tree on the Swiss data
data(swiss)
idx <- sample(1:nrow(swiss),as.integer(0.7*nrow(swiss)))
train <- swiss[idx,]
test <- swiss[-idx,]
library(rpart)
model <- rpart(Infant.Mortality ~ .,train)
preds <- predict(model,test)
## calculate mae and rmse
regrEval(test[,'Infant.Mortality'],preds,stats=c('mae','rmse'))
## calculate all statistics
regrEval(test[,'Infant.Mortality'],preds,train.y=train[,'Infant.Mortality'])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
