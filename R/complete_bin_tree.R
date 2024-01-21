#' Complete Binary Tree Finder
#'
#' Given a compositional dataset, finds the best fitting cascading binary
#' nested Dirichlet Tree with regards to log likelihood.
#'
#' @param dataset  A dataset of compositional data.  Each row should sum to 1 and
#' the number of columns should be equal to length of the alpha vector. The
#' dataset should not contain a header row or identifier column.
#'
#' @return A matrix where every two rows displays the ID of the nodes in the left
#' branch and right branch of the Dirichlet nesting trees.
#' @export



complete.bin.tree <- function(data.set){
  eps <- 10^-5
  data.set <- (data.set+eps)/(1+2*eps)
  data.set <- data.set/rowSums(data.set)
  data <- as.matrix(data.set)  ##puts data into correct form

  ## Initialize vectors
  direction.mat <- c()
  splits.mat <- c()
  splits.temp <- c()
  dummy <- c()

  j = 1
  first.index <- 1:ncol(data)  ##for the first binary split, we are going to use all the variables.

  splits.mat <- rbind(splits.mat, tree.finder(first.index, data))  ##Records left and right splits
  splits.temp <- splits.mat
  total <- sum(splits.mat)  ##If total equals 0, the non-nested design is the winner

  ## For cases where a nested design wins.

  if( total != 0){
    direction.temp <- matrix( c(1,2,1,3), byrow = TRUE, nrow=2, ncol=2)  ## What does this do?
    direction.mat <- direction.temp
  }

  ## The iterative step.
  while ( total != 0){
    dummy <- c()
    ## for each row of current splits.  Note there are 2*number of divisions in splits.
    ## one for the left branch and one for the right branch
    for (i in 1:nrow(splits.temp)){
      dummy <- rbind( dummy, tree.finder(splits.temp[i,], data))  ##does one more iteration on each split
    }

    total <- sum(dummy)
    splits.temp <- dummy
    splits.mat <- rbind(splits.mat, splits.temp)
    j = j+1
  }
  non.zero <- which(rowSums(splits.mat) != 0)
  splits.mat <- splits.mat[non.zero ,]
  return(splits.mat)
}
