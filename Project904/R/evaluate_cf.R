#' @title Evaluate the causal forest fitting
#' @description This function accepts a causal forest fitting object and calculate the MSE and coverage rate
#' @author Jiacheng He
#' @export
#' @import dplyr
#' @import grf
#'
#' @param cf A causal forest object
#' @param df A data frame which we evaluate the causal forest on
#' @param alpha The p value of the confidence interval
#'
#' @return A list of MSE and coverage rate
#' @examples
#' evaluate_df(cf_1, test_set)


evaluate_cf <- function(cf, df, alpha=0.05) {

  tau <- df$tau
  fit <- predict(cf, df$X, estimate.variance = TRUE)

  tau_hat <- fit$predictions %>% as.vector()
  sigma_hat <- sqrt(fit$variance.estimates) %>% as.vector()

  MSE <- mean((df$tau - tau_hat) ^ 2)

  tau_upper <- tau_hat + sigma_hat * qnorm(1 - alpha/2)
  tau_lower <- tau_hat - sigma_hat * qnorm(1 - alpha/2)
  coverage <- sum((tau <= tau_upper) & (tau >= tau_lower)) / length(tau)

  return(list(MSE = MSE, coverage = coverage))
}
