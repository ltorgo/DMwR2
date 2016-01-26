\name{sigs.PR}
\alias{sigs.PR}

\title{
 Precision and recall of a set of predicted trading signals
}
\description{
  This function calculates the values of Precision and Recall of a set
 of predicted signals, given the set of true signals. The function
 assumes three types of signals: 'b' (Buy), 's' (Sell) and 'h'
 (Hold). The function returns the values of Precision and Recall for the
 buy, sell and sell+buy signals.
}
\usage{
sigs.PR(preds, trues)
}

\arguments{
  \item{preds}{
A factor with the predicted signals (values should be 'b','s', or 'h')
}
\item{trues}{
  A factor with the predicted signals (values should be 'b','s', or 'h')
}
}
\details{
  Precision and recall are two evaluation statistics often used to
  evaluate predictions for rare events. In this case we are talking
  about buy and sell opportunities.

  Precision is the proportion of the events signaled by a model that
  actually occurred. Recall is a proportion of events that occurred that
  the model was able to capture. Ideally, the models should aim to
  obtain 100\% precision and recall. However, it is often the case that
  there is a trade-off between the two statistics.
}
\value{
  A matrix with three rows and two columns. The columns are the values
  of Precision and Recall, respectively. The rows are the values for the
  three different events (sell, buy and sell+buy).
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{trading.signals}}, \code{\link{tradingEvaluation}}, \code{\link{trading.simulator}}
}
\examples{
## A simple illustrative example use with random signals
ind <- rnorm(sd=0.3,100)
sigs <- trading.signals(ind,b.t=0.1,s.t=-0.1)
indT <- rnorm(sd=0.3,100)
sigsT <- trading.signals(indT,b.t=0.1,s.t=-0.1)
sigs.PR(sigs,sigsT)
}

\keyword{models}

