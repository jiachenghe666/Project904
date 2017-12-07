#' @title Simulate a set of covariates
#' @description Simulate a set of covariates
#' @author Jiacheng He
#' @export
#' 
#' @param d Covariate dimension
#' @param n Sample size
#' @param cdf Covariate distribution, default to U(0,1)
#' 
#' @return A n by d matrix
#' @examples 
#' X <- build_covariates(10, 1000)
#'

build_covariates <- function(d, n, cdf="unif") {

  
  if (cdf == "unif") {X <- matrix(runif(n * d), nrow = n, ncol = d) 
  } else if (cdf == "norm") {
    X <- matrix(rnorm(n * d), nrow = n, ncol = d)
  } else {
    stop("You fuck up the X distribution")
  }

  colnames(X) <- paste0("X", 1:d)

  return(X)
}
