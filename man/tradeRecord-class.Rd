\name{tradeRecord-class}
\Rdversion{1.1}
\docType{class}
\alias{tradeRecord}
\alias{tradeRecord-class}
\alias{plot,tradeRecord-method}
\alias{show,tradeRecord-method}
\alias{summary,tradeRecord-method}

\title{Class "tradeRecord" }
\description{	 This is a class that contains the result of a call to
  the function trading.simulator(). It contains information on the
  trading performance of a set of signals on a given set of "future"
  market quotes.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{tradeRecord(...)}.
  These objects contain information on i) the trading variables for each
  day in the simulation period; ii) on the positions hold during this
  period; iii) on the value used for transaction costs; iv) on the
  initial capital for the simulation; v) on the function that implements
  the trading policy used in the simulation; and vi) on the list of
  parameters of this function.
}
\section{Slots}{
  \describe{
    \item{\code{trading}:}{Object of class \code{"xts"} containing the
      information on the trading activities through the testing
      period. This object has one line for each trading date. For each
      date it includes information on the closing price of the market
      ("Close"), on the order given at the end of that day ("Order"), on
    the money available to the trader at the end of that day ("Money"),
    on the number of stocks hold by the trader ("N.Stocks"), and on the
    equity at the end of that day ("Equity").}
    \item{\code{positions}:}{Object of class \code{"matrix"} containing
      the positions hold by the trader during the simulation
      period. This is a matrix with seven columns, with as many rows as
      the number of positions hold by the trader. The columns of this
      matrix contain the type of position ("pos.type"), the number of
      stocks of the position ("N.stocks"), the date when the position
      was opened ("Odate"), the open price ("Oprice"), the closing date
      ("Cdate"), the closing price ("Cprice") and the percentage return
      of the position ("result"). }
    \item{\code{trans.cost}:}{Object of class \code{"numeric"} with the
      monetary value of each transaction (market order). }
    \item{\code{init.cap}:}{Object of class \code{"numeric"} with the
      initial monetary value of the trader. }
    \item{\code{policy.func}:}{Object of class \code{"character"} with
      the name of the function that should be called at the end of each
      day to decide what to do, i.e. the trading policy function. This
      function is called with the vector of signals till the current
      date, the market quotes till today, the current position of the
      trader and the currently available money. }
    \item{\code{policy.pars}:}{Object of class \code{"list"} containing
      a list of extra parameters to be used when calling the trading
      policy function (these depend on the function defined by the user). }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "tradeRecord", y = "ANY")}: provides
    a graphical representation of the trading results.}
    \item{show}{\code{signature(object = "tradeRecord")}: shows an
      object in a proper way. }
    \item{summary}{\code{signature(object = "tradeRecord")}: provides a
      summary of the trading results. }
	 }
}
\references{
  Torgo, L. (2016) \emph{Data Mining using R: learning with case studies,
  second edition},
  Chapman & Hall/CRC (ISBN-13: 978-1482234893).

  \url{http://ltorgo.github.io/DMwR2}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{trading.simulator}}, \code{\link{tradingEvaluation}}
}
\examples{
showClass("tradeRecord")
}
\keyword{classes}
