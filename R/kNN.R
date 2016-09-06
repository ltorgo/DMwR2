# =================================================================
# This function provides a formula interface to the function
# knn() in package class, that implements k-nearest neighbours
# classifiers.
# On top of that function this adds the possibility of standardizing
# the data through parameter stand (see examples below)
# -----------------------------------------------------------------
# Mar 2010, Jan 2016, Luis Torgo
# -----------------------------------------------------------------
kNN <- function(form, train, test, stand=TRUE, stand.stats=NULL,...) {
  tgtCol <- which(colnames(train)==as.character(form[[2]]))
  if (stand) {
    if (is.null(stand.stats)) tmp <- scale(train[,-tgtCol],center=T,scale=T)
    else tmp <- scale(train[,-tgtCol],center=stand.stats[[1]],scale=stand.stats[[2]])
    train[,-tgtCol] <- tmp
    ms <- attr(tmp,"scaled:center")
    ss <- attr(tmp,"scaled:scale")
    test[,-tgtCol] <- scale(test[,-tgtCol],center=ms,scale=ss)
  }
  class::knn(train[,-tgtCol],test[,-tgtCol],train[,tgtCol],...)
}
