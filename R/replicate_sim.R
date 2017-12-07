#' @title Run one simulation of causal forest
#' @description This function accepts the key parameters and set up one simulation
#' @author Jiacheng He
#' @export
#' @import grf
#' 
#' @param d Covariate dimension
#' @param n_train Sample size of the training set
#' @param n_test Sample size of the test set
#' @param DGP Which data generating process to adopt
#' @param sample.fraction A parameter passed to causal_forest()
#' @param mtry A parameter passed to causal_forest()
#' @param num.trees A parameter passed to causal_forest()
#' @param min.node.size A parameter passed to causal_forest()
#' @param lambda A parameter passed to causal_forest()
#' @param cf_seed A parameter passed to causal_forest()
#'
#' @return The performance evaluation of the causal forest of this simulation (MSE and coverage rate)
#' @examples 
#' 

replicate_sim <- function(d=10, n_train=1000, n_test=1000, DGP=DGP_1, sample.fraction=0.5, mtry=ceiling(2*d/3), num.trees=2000, min.node.size=NULL, lambda=0, cf_seed=904) {
  
  X <- build_covariates(d = d, n = n_train + n_test)
  test_index <- sample(1:(n_train+n_test), n_test, replace = FALSE)
  X_test <- X[test_index, ]
  X_train <- X[-test_index, ]
  
  if (is.list(DGP)) {
    df_train <- generate_true_model(X_train, DGP_tau = DGP$tau, DGP_e = DGP$e, DGP_m = DGP$m)
    df_test <- generate_true_model(X_test, DGP_tau = DGP$tau, DGP_e = DGP$e, DGP_m = DGP$m)
  } else {
    stop("You fuck up the DGP")
  }
  
  cf <- causal_forest(df_train$X, df_train$Y, df_train$W, sample.fraction = sample.fraction, mtry = mtry,
                      num.trees = num.trees, min.node.size = min.node.size, lambda = lambda, seed = cf_seed)
  
  return(evaluate_cf(cf, df_test))
  
}