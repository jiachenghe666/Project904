#' @title Evaluate the MSE and coverage rate
#' @description This function receives the point predictions and true values
#' and compute the MSE and confidence coverage rate
#' @author Jiacheng He
#'
#' @import dplyr
#' @import grf
#' @export
#'
#'

evaluate_cf <- function(df_fit, alpha=0.05) {

  df_fit <- df_fit %>%
    mutate(tau_upper = tau_hat + sigma_hat * qnorm(1 - alpha/2),
           tau_lower = tau_hat - sigma_hat * qnorm(1 - alpha/2))

  MSE <- df_fit %>%
    group_by(param, rep) %>%
    summarise(MSE = mean((tau-tau_hat)^2))

  coverage <- df_fit %>%
    group_by(param, point) %>%
    summarise(coverage = mean((tau <= tau_upper) & (tau >= tau_lower)))

  return(list(MSE = MSE, coverage = coverage))
}
