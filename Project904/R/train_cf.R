#' @title Train causal forest
#' @description This function accepts the key parameters and set up one training simulation
#' @author Jiacheng He
#' @export
#' @import dplyr
#' @import grf
#'
#' @param d Covariate dimension
#' @param n_train Sample size of the training set
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

train_cf <- function(DGP, param, param_type, cf_seed=904) {

  if (param_type == "n") {
    n_train <- param
  } else {n_train <- 1000}

  if (param_type == "d") {
    d <- param
  } else {d <- 10}

  df_train <- generate_true_model(DGP, d, n_train)

  causal_forest_2 <- partial(causal_forest, X = df_train$X, Y = df_train$Y, W = df_train$W)

  if (param_type == "sample.fraction") {
    cf <- causal_forest_2(sample.fraction = param)
  } else if (param_type == "mtry") {
    cf <- causal_forest_2(mtry = param)
  } else if (param_type == "num.trees") {
    cf <- causal_forest_2(num.trees = param)
  } else if (param_type == "min.node.size") {
    cf <- causal_forest_2(min.node.size = param)
  } else if (param_type == "lambda") {
    cf <- causal_forest_2(lambda = param)
  } else {
    cf <- causal_forest_2()}

  return(cf)

}
