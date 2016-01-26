################################################################## 
# THIS FILE CONTAINS FUNCTIONS THAT ARE RELATED TO OBTAINING     #
# TREE-BASED MODELS WITH THE RPART PACKAGE. IT BASICALLY PROVIDES#
# SOME EXTRA FUNCTIONALITY.                                      #
# IT IS A PART OF THE PACKAGE DMwR                               #
##################################################################
# Author : Luis Torgo (ltorgo@inescporto.pt)     Date: Jan 2009  #
# License: GPL (>= 2)                                            #
##################################################################



# =====================================================
# Function that obtains a tree-based model using the
# x-SE post pruning rule of CART (Breiman et al. 1984).
# The idea is to grow an overly large tree and then post
# prune it using the internal cross validation estimates
# obtained by the initial call to rpart(), which are
# accessible through the cptable component of rpart objects.
# =====================================================
# Luis Torgo, Jan 2009, Jan 2016
# =====================================================
rpartXse <- function(form,data,se=1,cp=0,minsplit=6,verbose=F,...) {
#   require(rpart)
   tree <- rpart::rpart(form,data,cp=cp,minsplit=minsplit,...)
   if (verbose && ncol(tree$cptable) < 5) 
     warning("No pruning will be carried out because no estimates were obtained.")
   rt.prune(tree,se,verbose)
 }

#
# Helper function to actually carry out the prunning
rt.prune <- function(tree,se=1,verbose=T,...) {
   if (ncol(tree$cptable) < 5) tree
   else {
     lin.min.err <- which.min(tree$cptable[,4])
     if (verbose && lin.min.err == nrow(tree$cptable))
       warning("Minimal Cross Validation Error is obtained 
                at the largest tree.\n  Further tree growth 
               (achievable through smaller 'cp' parameter value),\n
                could produce more accurate tree.\n")
     tol.err <- tree$cptable[lin.min.err,4] + se * tree$cptable[lin.min.err,5]
     se.lin <- which(tree$cptable[,4] <= tol.err)[1]
     rpart::prune.rpart(tree,cp=tree$cptable[se.lin,1]+1e-9)
   }
}



