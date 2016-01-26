
# =====================================================
# Function that can be used to to apply self training on
# a given classifier.
# =====================================================
# Luis Torgo, Feb 2010, Jan 2016
# =====================================================
# Example run:
#
SelfTrain <- function(form,data,
                      learner, learner.pars=list(),
                      pred, pred.pars=list(),
                      thrConf=0.9,
                      maxIts=10,percFull=1,
                      verbose=FALSE)
  {
    N <- NROW(data)
    it <- 0
    sup <- which(!is.na(data[,as.character(form[[2]])]))
    repeat {
      it <- it+1
      model <- do.call(learner,c(list(form,data[sup,]),learner.pars))
      probPreds <- do.call(pred,c(list(model,data[-sup,]),pred.pars))
      new <- which(probPreds[,2] > thrConf)
      if (verbose) cat('IT.',it,'\t nr. added exs. =',length(new),'\n')
      if (length(new)) {
        data[(1:N)[-sup][new],as.character(form[[2]])] <- probPreds[new,1]
        sup <- c(sup,(1:N)[-sup][new])
      } else break
      if (it == maxIts || length(sup)/N >= percFull) break
    }
    return(model)
  }
