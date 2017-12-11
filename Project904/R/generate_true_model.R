#' @title Generate the components of the variables
#' @description This function accepts the baseline covariates and data generating process,
#' then return tau, m, and e, then simulate Y
#' @author Jiacheng He
#' @import dplyr
#' @export
#'
#' @param X The covariate matrix
#' @param DGP_tau An expression, the DGP for tau
#' @param DGP_e An expression, the DGP for e
#' @param DGP_m An expression, the DGP for m
#' @param noise_sd The standard deviation of the Gaussian noise of the model
#'
#' @return A list of X, Y, W, tau, e, and m. Prepare for the subsequent use of model fitting and evaluation.
#' @examples
#' data <- generate_true_model(X, DGP1$tau, DGP1$e, DGP$m)
#'


generate_true_model <- function(X, DGP_tau, DGP_e, DGP_m, noise_sd=1) {

  n <- dim(X)[1]

  if (is.numeric(DGP_tau)) {tau <- rep(DGP_tau, n)
  } else if (is.expression(DGP_tau)) {
    tau <- eval(DGP_tau, envir = as_data_frame(X))
  } else {
    stop("You fuck up the tau")
  }

  if (is.numeric(DGP_e)) {e <- rep(DGP_e, n)
  } else if (is.expression(DGP_e)) {
    e <- eval(DGP_e, envir = as_data_frame(X))
  } else {
    stop("You fuck up the e")
  }

  if (is.numeric(DGP_m)) {m <- rep(DGP_m, n)
  } else if (is.expression(DGP_m)) {
    m <- eval(DGP_m, envir = as_data_frame(X))
  } else {
    stop("You fuck up the m")
  }

  W <- rbinom(n, size = 1, prob = e)
  Y <- m + tau*W/2 - tau*(1-W)/2 + rnorm(n, sd = noise_sd)

  return(list(X = as.matrix(X), Y = Y, W = W, tau = tau, e = e, m = m))
}
