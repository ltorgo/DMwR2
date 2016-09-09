

# =====================================================
# Function that obtains a statistic of centrality of a
# variable, given a sample of values.
# If the variable is numeric it returns de median, if it
# is a factor it returns the mode. In other cases it
# tries to convert to a factor and then returns the mode.
# =====================================================
# Luis Torgo, Jan 2009
# =====================================================
centralValue <- function(x, ws=NULL) {
    x <- unlist(x)  # because of dplyr structures not dropping (errors with dat[,i])
    if (is.numeric(x)) {
        if (is.null(ws)) median(x,na.rm=TRUE)
        else if ((s <- sum(ws)) > 0) sum(x*(ws/s)) else NA
    } else {
        x <- as.factor(x)
        if (is.null(ws)) levels(x)[which.max(table(x))]
        else levels(x)[which.max(aggregate(ws,list(x),sum)[,2])]
    }
}


# =====================================================
# Small utility function to obtain the target (response)
# variable values corresponding to a given formula and
# data set.
# =====================================================
# Luis Torgo, Jan 2009
# =====================================================
resp <- function(formula,data) model.response(model.frame(formula, data))


# =====================================================
# Small utility function to obtain the number of the rows
# in a data frame that have either a "large" number of 
# unknown values.
# "Large" can be defined either as a proportion of the
# number of columns or as the number in itself.
# =====================================================
# Luis Torgo, Mar 2009, Mar 2011
# =====================================================
manyNAs <- function(data,nORp=0.2) {
  n <- if (nORp < 1) as.integer(nORp*ncol(data)) else round(nORp,0)
  idxs <- which(apply(data,1,function(x) sum(is.na(x))) > n)
  if (!length(idxs)) warning('Empty index generated, no rows with many NAs. Undesirable effects may be caused if indexing a data frame with this.')
  idxs
}



# =====================================================
# Function that fills in all unknowns using the statistic
# of centrality of the respective column.
# This statistic is either the median for numeric columns
# or the mode for nominal variables.
# =====================================================
# Luis Torgo, Mar 2009
# =====================================================
centralImputation <- function(data) {
  for(i in seq(ncol(data))) 
    if (any(idx <- is.na(data[,i])))
      data[idx,i] <- centralValue(data[,i])
  data
}


# =====================================================
# Function that fills in all unknowns using the k Nearest
# Neighbours of each case with unknows. 
# By default it uses the values of the neighbours and 
# obtains an weighted (by the distance to the case) average
# of their values to fill in the unknows.
# If meth='median' it uses the median/most frequent value,
# instead.
# =====================================================
# Luis Torgo, Mar 2009, Nov 2011
# =====================================================
knnImputation <- function(data,k=10,scale=TRUE,meth='weighAvg',distData=NULL) {

    n <- nrow(data)  
    if (!is.null(distData)) {
        distInit <- n+1
        data <- rbind(data,distData)
    } else distInit <- 1
    N <- nrow(data)
    
    ncol <- ncol(data)
    ##nomAttrs <- rep(F,ncol)
    ##for(i in seq(ncol)) nomAttrs[i] <- is.factor(data[,i])
    ##nomAttrs <- which(nomAttrs)
    ##contAttrs <- setdiff(seq(ncol),nomAttrs)
    contAttrs <- which(vapply(data,dplyr::type_sum,character(1)) %in% c("dbl","int"))
    nomAttrs <- setdiff(seq.int(ncol),contAttrs)
    hasNom <- length(nomAttrs)

    dm <- data
    if (scale) dm[,contAttrs] <- scale(dm[,contAttrs])
    if (hasNom)
        for(i in nomAttrs) dm[[i]] <- as.integer(dm[[i]])
    
    dm <- as.matrix(dm)
    
    nas <- which(!complete.cases(dm))
    if (!is.null(distData)) tgt.nas <- nas[nas <= n]
    else tgt.nas <- nas
    
    if (length(tgt.nas) == 0)
        warning("No case has missing values. Stopping as there is nothing to do.")
    
    xcomplete <- dm[setdiff(distInit:N,nas),]
    if (nrow(xcomplete) < k)
        stop("Not sufficient complete cases for computing neighbors.")
    
    for (i in tgt.nas) {
        
        tgtAs <- which(is.na(dm[i,]))
        
        dist <- scale(xcomplete,dm[i,],FALSE)
        
        xnom <- setdiff(nomAttrs,tgtAs)
        if (length(xnom)) dist[,xnom] <-ifelse(dist[,xnom]>0,1,dist[,xnom])
        
        dist <- dist[,-tgtAs]
        dist <- sqrt(drop(dist^2 %*% rep(1,ncol(dist))))
        ks <- order(dist)[seq(k)]
        for(j in tgtAs)
            if (meth == 'median')
                data[i,j] <- centralValue(data[setdiff(distInit:N,nas)[ks],j])
            else 
                data[i,j] <- centralValue(data[setdiff(distInit:N,nas)[ks],j],exp(-dist[ks]))
    }
    
    data[1:n,]
}




# =====================================================
# Function that inverts the effect of the scale function
# =====================================================
# Luis Torgo, Nov 2009
# =====================================================
unscale <- function(vals,norm.data,col.ids) {
  cols <- if (missing(col.ids)) 1:NCOL(vals) else col.ids
  if (length(cols) != NCOL(vals)) stop('Incorrect dimension of data to unscale.')
  centers <- attr(norm.data,'scaled:center')[cols]
  scales <- attr(norm.data,'scaled:scale')[cols]
  unvals <- scale(vals,center=(-centers/scales),scale=1/scales)
  attr(unvals,'scaled:center') <- attr(unvals,'scaled:scale') <- NULL
  unvals
}





# ======================================================================
# Function for normalizing the range of values of a continuous variable.
# Taken from the book "Data preparation for data mining" by Dorian Pyle
# (pp. 271-274)
#
# This function ensures all values will be between 0 and 1.
#
# 13/05/2002, Luis Torgo.
# ----------------------------------------------------------------------
# Example :
# SoftMax(algae[,'NO3'])
# the following obtains the transformation just for one value
# SoftMax(45.23,avg=mean(algae[,'NO3'],na.rm=T),std=sd(algae[,'NO3'],na.rm=T))
#
# Note:
# The lambda parameter controls the range of values that gets a linear
# mapping. It represents the number of standard deviations that should be
# included in the linear mapping region (e.g. 1-> 68% of the distribution gets
# linear mapping, while 2-> 95.5%, 3 -> 99.7%, etc.)
SoftMax <- function(x,lambda=2,avg=mean(x,na.rm=T),std=sd(x,na.rm=T))
{
  if (is.data.frame(x) | is.array(x)) return(apply(x,2,SoftMax,lambda))
  vt <- (x-avg)/(lambda*(std/(2*pi)))
  1/(1+exp(-vt))
}



# ======================================================================
# Function for performing a linear scaling transformation
#
# This function ensures values between 0 to 1 (except for out of the sample
# values, for that use SoftMax).
#
# 13/05/2002, Luis Torgo.
# ----------------------------------------------------------------------
# Example :
# LinearScaling(algae[,'NO3'])
# the following obtains the transformation just for one value
# LinearScaling(45.23,mx=max(algae[,'NO3'],na.rm=T),mn=min(algae[,'NO3'],na.rm=T))
#
LinearScaling <- function(x,mx=max(x,na.rm=T),mn=min(x,na.rm=T))
  (x-mn)/(mx-mn)


# ======================================================================
# Function performs a change of scale
#
# This function ensures values between a given scale
#
# 2/04/2003, Luis Torgo.
# ----------------------------------------------------------------------
#
ReScaling <- function(x,t.mn,t.mx,d.mn=min(x,na.rm=T),d.mx=max(x,na.rm=T)) {
  sc <- (t.mx-t.mn)/(d.mx-d.mn)
  sc*x + t.mn - sc*d.mn
}


# ======================================================================
# Function for creating an embeded data set from an univariate time series
#
#
# 2016/09/09, Luis Torgo.
# ----------------------------------------------------------------------
#
createEmbedDS <- function(s, emb=4) {
    d <- dim(s)
    if (!is.null(d) && d[2] > 1) stop("Only applicable to uni-variate time series")
    if (emb < 2 || emb > length(s)) stop("Invalid embed size")
    e <- embed(s,emb)
    colnames(e) <- c("T",paste("T",1:(emb-1),sep="_"))
    if (xts::is.xts(s)) return(xts::xts(e,index(s)[emb:length(s)])) else return(e)
}


# ======================================================================
# Function for counting the nr of lines of a big CSV file
#
#
# 2016/09/09, Luis Torgo.
# ----------------------------------------------------------------------
#
nrLinesFile <- function(f) {
    if (.Platform$OS.type == "unix") 
        as.integer(strsplit(trimws(system(paste("wc -l",f),intern=TRUE)),
                            " ")[[1]][1]) 
    else 
        stop("This function requires unix-based systems")
}


# ======================================================================
# Function for drawing a random sample of lines from a big CSV file
#
#
# 2016/09/09, Luis Torgo.
# ----------------------------------------------------------------------
#
sampleCSV <- function(file, percORn, nrLines, header=TRUE, mxPerc=0.5) {
    if (.Platform$OS.type != "unix") 
        stop("This function requires unix-based systems")
    
    if (missing(nrLines)) nrLines <- nrLinesFile(file)
    
    if (percORn < 1)
        if (percORn > mxPerc) 
            stop("This function is not adequate for that big samples.")
        else percORn <- as.integer(percORn*nrLines)
    perc <- min(2*percORn/nrLines, mxPerc)
    
    system(paste0("perl -ne 'print if (rand() < ",perc,")' ",file,
                  " > ",file,".tmp.csv"))
    dt <- readr::read_csv(paste0(file,".tmp.csv"),col_names=header, n_max=percORn)
    file.remove(paste0(file,".tmp.csv"))
    if (nrow(dt) != percORn) 
        warning(paste("Expecting",percORn,"rows, but got",nrow(dt)))
    dt
}


# ======================================================================
# Function for drawing a random sample of lines from a big database table
#
#
# 2016/09/09, Luis Torgo.
# ----------------------------------------------------------------------
#
sampleDBMS <- function(dbConn, tbl, percORn, mxPerc=0.5) {
    nrRecords <- unlist(dbGetQuery(dbConn, paste("select count(*) from",tbl)))
    
    if (percORn < 1)
        if (percORn > mxPerc) 
            stop("This function is not adequate for that big samples.")
        else percORn <- as.integer(percORn*nrRecords)
    perc <- min(2*percORn/nrRecords, mxPerc)
    
    dt <- dbGetQuery(dbConn,paste("select * from (select * from",tbl,
                                  "where rand() <= ",perc,") as t limit ",percORn))
    if (nrow(dt) != percORn) 
        warning(paste("Expecting",percORn,"rows, but got",nrow(dt)))
    dt
}
