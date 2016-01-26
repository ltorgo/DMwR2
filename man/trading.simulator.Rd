\name{trading.simulator}
\alias{trading.simulator}

\title{
Simulate daily trading using a set of trading signals
}
\description{
  This function can be used to obtain the trading performance of a set
  of signals by simulating daily trading on a market with these signals
  according to a user-defined trading policy. The idea is that the user
  supplies the actual quotes for the simulation period together with the
  trading signals to use during this period. On top of that the user
  also supplies a function implementing the trading policy to use. The
  result is a trading record for this period. This result can then be
  inspected and used to obtain several trading performance metrics with
  other functions.
}
\usage{
trading.simulator(market, signals,
                  policy.func, policy.pars = list(),
                  trans.cost = 5, init.cap = 1e+06)
}

\arguments{
  \item{market}{
    A \code{xts} object containing the market quotes for each day of the simulation
  period. This object should contain at least the Open, High, Low and
  Close quotes for each day. These quotes (with these exact names) are
  used within the function and thus are required.
}
\item{signals}{
  A factor with as many signals as there are rows in the \code{market}
  \code{xts} object, i.e. as many signals as there are trading days in
  the simulation period. The signals should be 'b' for Buy, 's' for Sell
  and 'h' for Hold (actually this information is solely processed within
  the user-defined trading policy function which means that the values may be
  whatever the writer of this function wants).
}
  \item{policy.func}{
A string with the name of the function that will be called at the end of
  each day of the trading period. This user-defined function implements
  the trading policy to be used in the simulation. See the Details section for
  understanding what is the task of this function.
}
  \item{policy.pars}{
A list with parameters that are passed to the user-defined trading
  policy function when it is called at the end of each day.
}
\item{trans.cost}{
  A number with the cost of each market transaction (defaults to 5
  monetary units).
}
  \item{init.cap}{
A number with the initial amount of money available for trading at the start of the
  simulation period (defaults to 1,000,000 monetary units).
}
}
\details{
  This function can be used to simulate daily trading according to a set
  of signals. The main parameters of this function are the market quotes
  for the simulation period and the model signals for this period. Two
  other parameters are the name of the user-defined trading policy
  function and its list of parameters. Finally, we can also specify the
  cost of each transaction and the initial capital available for the
  trader. The simulator will call the user-provided trading policy
  function at the end of each daily section, and the function should
  return the orders that it wants the simulator to carry out. The
  simulator carries out these orders on the market and records all
  activity on several data structures. The result of the simulator is an
  object of class \code{tradeRecord} containing the information of this
  simulation. This object can then be used in other functions to obtain
  economic evaluation metrics or graphs of the trading activity.

  The key issue in using this function is to create the user-defined
  trading policy function. These functions should be written using a
  certain protocol, that is, they should be aware of how the simulator
  will call them, and should return the information this simulator is
  expecting. At the end of each daily session d, the simulator calls the
  trading policy function with four main arguments plus any other
  parameters the user has  provided in the call to the simulator in the
  parameter \code{policy.pars}. These four arguments are (1) a vector
with the predicted signals until day d, (2) the market quotes (up to d), (3)
the currently opened positions, and (4) the money currently available to the
trader. The current positions are stored in a matrix with as many rows as there are open
positions at the end of day d. This matrix has four columns: "pos.type" that
can be 1 for a long position or -1 for a short position; "N.stocks", which
is the number of stocks of the position; "Odate", which is the day on which
the position was opened (a number between 1 and d); and "Oprice", which
is the price at which the position was opened. The row names of this matrix
contain the IDs of the positions that are relevant when we want to indicate
the simulator that a certain position is to be closed.
All this information is provided by the simulator to ensure the user can
  define a broad set of trading policy functions. The user-defined functions should
return a data frame with a set of orders that the simulator should carry out.
This data frame should include the following information (columns): "order",
which should be 1 for buy orders and -1 for sell orders; "order.type", which
should be 1 for market orders that are to be carried out immediately
  (actually at next day open price),  2 for limit orders or 3 for stop orders; "val",
which should be the quantity of stocks to trade for opening market orders, NA
for closing market orders, or a target price for limit and stop orders; "action",
which should be "open" for orders that are opening a new position or "close" for
orders closing an existing position; and finally, "posID", which should contain
the ID of the position that is being closed, if applicable.

}
\value{
  An object of class \code{tradeRecord} (see 'class?tradeRecord' for details).
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{tradingEvaluation}}, \code{\linkS4class{tradeRecord}},
\code{\link{trading.signals}},  \code{\link{sigs.PR}}
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
  summary(tradeR)
  tradingEvaluation(tradeR)
}
  ## See the performance graphically
  \dontrun{
    plot(tradeR,market)
  }

}
\keyword{models}

