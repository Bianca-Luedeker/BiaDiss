#' Computes the output of the log likelihood function for a Dirichlet
#' distribution given the alpha parameters and a dataset.
#'
#'
#' @param alpha A vector of alpha parameters for the Dirichlet distribution.
#' @param data  A dataset of compositional data.  Each row should sum to 1 and
#' the number of columns should be equal to length of the alpha vector. The
#' dataset should not contain a header row or identifier column.
#'
#' @return The height of the log likelihood function at the specified alpha
#' parameters and dataset.
#' @export

log.like.1 <- function(alpha, data){
  n <- nrow(data)
  log.p.bar <- apply(log(data), 2, mean)
  result <- n*lgamma(sum(alpha)) - n*sum(lgamma(alpha)) + n*sum(alpha*log.p.bar)
  return(result)
}
