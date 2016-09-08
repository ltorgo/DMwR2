################################################################# 
# THIS FILE CONTAINS THE CLASSES AND RESPECTIVE METHODS OF THE  #
# PACKAGE DMwR                                                  #
#################################################################
# Author : Luis Torgo (ltorgo@dcc.fc.up.pt)                     #
# Date: Jan 2009, Jan 2016                                      #
# License: GPL (>= 2)                                           #
#################################################################

# Defined classes :
#   tradeRecord





# ==============================================================
# CLASS: tradeRecord
# ==============================================================
# Luis Torgo, Nov 2009
# ==============================================================


# --------------------------------------------------------------
# class def
setClass("tradeRecord",
         representation(trading="zoo",
                        positions="matrix",
                        trans.cost="numeric",
                        init.cap="numeric",
                        policy.func="character",
                        policy.pars="list")
         )


# --------------------------------------------------------------
# constructor function
tradeRecord <- function(t,p,tc,c,pf,pp) {
  o <- new("tradeRecord")
  o@trading <- t
  o@positions <- p
  o@trans.cost <- tc
  o@init.cap <- c
  o@policy.func <- pf
  o@policy.pars <- pp
  o
}



# ==============================================================
# METHODS: tradeRecord
# ==============================================================
# Luis Torgo, Nov 2009, Jan 2016
# ==============================================================

# --------------------------------------------------------------
# plot
#
.Eq <- function(p) p[,'Equity']
.St <- function(p) p[,'N.Stocks']
.addEq <- quantmod::newTA(FUN = .Eq, col = 'red', legend = "Equity")
.addSt <- quantmod::newTA(FUN = .St, col = 'green', legend = "N.Stocks")

#.addEq <- quantmod::newTA(FUN = function(p) p[,'Equity'], col = 'red', legend = "Equity")
#.addSt <- quantmod::newTA(FUN = function(p) p[,'N.Stocks'], col = 'green', legend = "N.Stocks")

setMethod("plot", "tradeRecord",
  function(x,y,verbose=TRUE,...) {
      tr <- x
      market <- cbind(y,zoo::coredata(x@trading)[,c('Equity','N.Stocks')])
      quantmod::candleChart(market,
                            TA=c(.addEq(),  .addSt()),
#                            TA=c(quantmod::newTA(FUN = .Eq, col = 'red', legend = "Equity") ,
#                                 quantmod::newTA(FUN = .St, col = 'green', legend = "N.Stocks")),
                           ,...)
      if (verbose)
          cat('Rentability = ',100*(zoo::coredata(market[nrow(market),'Equity'])/
                                    zoo::coredata(market[1,'Equity'])-1),'%\n')
  }

)

# --------------------------------------------------------------
# summary
#
setMethod("summary", "tradeRecord",
  function(object) {
      cat('\n== Summary of a Trading Simulation with ',nrow(object@trading),' days ==\n')
      cat('\nTrading policy function : ',object@policy.func,'\n')
      cat('Policy function parameters:\n')
      for(x in names(object@policy.pars))
          cat('\t',x,' = ',deparse(object@policy.pars[[x]]),'\n')
      cat('\n')
      cat('Transaction costs : ',object@trans.cost,'\n')
      cat('Initial Equity    : ',round(object@init.cap,1),'\n')
      cat('Final Equity      : ',round(object@trading[nrow(object@trading),'Equity'],1),'  Return : ',
          round(100*(object@trading[nrow(object@trading),'Equity']/object@init.cap - 1),2),'%\n')
      cat('Number of trading positions: ',NROW(object@positions),'\n')
      cat('\nUse function "tradingEvaluation()" for further stats on this simulation.\n\n')
  }
)


# --------------------------------------------------------------
# show
#
setMethod("show","tradeRecord",
          function(object) {
            cat('\nObject of class tradeRecord with slots:\n\n')
            cat('\t trading: <xts object with a numeric ',
                dim(object@trading)[1],'x',dim(object@trading)[2],' matrix>\n')
            cat('\t positions: <numeric ',dim(object@positions)[1],'x',dim(object@positions)[2],' matrix>\n')
            cat('\t init.cap : ',object@init.cap,'\n')
            cat('\t trans.cost : ',object@trans.cost,'\n')
            cat('\t policy.func : ',object@policy.func,'\n')
            cat('\t policy.pars : <list with ',length(object@policy.pars),' elements>\n\n')
          })





