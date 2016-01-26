\name{tradingEvaluation}
\alias{tradingEvaluation}
\title{
Obtain a set of evaluation metrics for a set of trading actions
}
\description{
  This function receives as argument an object of class
  \code{tradeRecord} that is the result of a call to the
  \code{trading.simulator()} function and produces a set of evaluation
  metrics of this simulation
}
\usage{
tradingEvaluation(t)
}

\arguments{
  \item{t}{
An object of call \code{tradeRecord} (see 'class?tradeRecord' for details)
}
}
\details{
  Given the result of a trading simulation this function calculates:
  \itemize{
    \item The number of trades.
    \item The number of profitable trades.
    \item The percentage of profitable trades.
    \item The profit/loss of the simulation (i.e. the final result).
    \item The return of the simulation.
    \item The return over the buy and hold strategy.
    \item The maximum draw down of the simulation.
    \item The Sharpe Ration score.
    \item The average percentage return of the profitable trades.
    \item The average percentage return of the non-profitable trades.
    \item The average percentage return of all trades.
    \item The maximum return of all trades.
    \item The maximum percentage loss of all trades.
    }

}
\value{
  A vector of evaluation metric values
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\linkS4class{tradeRecord}},  \code{\link{trading.simulator}}, \code{\link{trading.signals}}
}
\examples{
## An example partially taken from chapter 3 of my book Data Mining
## with R (Torgo,2010)

## First a trading policy function
## This function implements a strategy to trade on futures with
## long and short positions. Its main ideas are the following:
## - all decisions aretaken at the end of the day, that is, after
## knowing all daily quotes of the current session.
## - if at the end of day d our models issue a sell signal and  we
## currently  do not hold any opened position, we will open a short
## position  by issuing a sell order. When this order is carried out  by
## the market at a price  pr sometime in the  future, we will
## immediately post two other orders. The first is a buy limit order
## with  a limit price of pr - p\%, where p\% is a target profit margin.
## We will wait 10 days for this target to be reached. If the  order  is
## not carried out by this deadline, we will buy at the closing price
## of  the 10th day. The second order is a buy stop order with a  price
## limit  pr + l\%. This order is placed with the goal of limiting our
## eventual  losses with this position. The order will be executed if
## the  market reaches the price pr + l\%, thus limiting our possible
## losses  to l\%.
## - if the end of the day signal is buy the strategy is more or less
## the inverse
\dontrun{
library(xts)
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
                     )
  {
    d <- NROW(market) # this is the ID of today
    orders <- NULL
    nOs <- NROW(opened.pos)
    # nothing to do!
    if (!nOs && signals[d] == 'h') return(orders)

    # First lets check if we can open new positions
    # i) long positions
    if (signals[d] == 'b' && !nOs) {
      quant <- round(bet*money/market[d,'Close'],0)
      if (quant > 0) 
        orders <- rbind(orders,
              data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1+exp.prof),
                                 market[d,'Close']*(1-max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )

    # ii) short positions  
    } else if (signals[d] == 's' && !nOs) {
      # this is the nr of stocks we already need to buy 
      # because of currently opened short positions
      need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                                 "N.stocks"])*market[d,'Close']
      quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
      if (quant > 0)
        orders <- rbind(orders,
              data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1-exp.prof),
                                 market[d,'Close']*(1+max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )
    }
    
    # Now lets check if we need to close positions
    # because their holding time is over
    if (nOs) 
      for(i in 1:nOs) {
        if (d - opened.pos[i,'Odate'] >= hold.time)
          orders <- rbind(orders,
                data.frame(order=-opened.pos[i,'pos.type'],
                           order.type=1,
                           val = NA,
                           action = 'close',
                           posID = rownames(opened.pos)[i]
                          )
                         )
      }

    orders
  }

  ## Now let us play a bit with the SP500 quotes availabe in our package
  data(GSPC)

  ## Let us select the last 3 months as the simulation period
  market <- last(GSPC,'3 months')
  
  ## now let us generate a set of random trading signals for
  ## illustration purpose only
  ndays <- nrow(market)
  aRandomIndicator <- rnorm(sd=0.3,ndays)
  theRandomSignals <- trading.signals(aRandomIndicator,b.t=0.1,s.t=-0.1)

  ## now lets trade!
  tradeR <- trading.simulator(market,theRandomSignals,
              'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=10))

  ## a few stats on the trading performance
  tradingEvaluation(tradeR)
}
}

\keyword{models}

