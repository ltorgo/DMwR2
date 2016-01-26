\name{trading.signals}
\alias{trading.signals}

\title{
  Discretize a set of values into a set of trading signals
}
\description{
  This function transforms a set of numeric values into a set of trading
  signals according to two thresholds: one that establishes the limit
  above which any value will be transformed into a buy signal ('b'), and
  the other that sets the value below which we have a sell signal
  ('s'). Between the two thresholds we will have a hold signal ('h').
}
\usage{
trading.signals(vs, b.t, s.t)
}

\arguments{
  \item{vs}{
A vector with numeric values
}
  \item{b.t}{
A number representing the buy threshold
}
\item{s.t}{
  A number representing the sell threshold
}
}
\value{
A factor with three possible values 'b' (buy), 's' (sell) or 'h' (hold)
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
trading.signals(rnorm(sd=0.5,100),b.t=0.1,s.t=-0.12)
}

\keyword{models}

