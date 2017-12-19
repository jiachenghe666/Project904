#' @title Evaluate the causal forest fitting
#' @description This function accepts a causal forest fitting object and calculate the MSE and coverage rate
#' @author Jiacheng He
#' @export
#' @import dplyr
#' @import grf
#'
#' @param cf A causal forest object
#' @param df_test A test set data frame which we evaluate the causal forest on
#' @param alpha The p value of the confidence interval
#'
#' @return A list of MSE and coverage rate
#' @examples
#' evaluate_cf(cf_1, test_set)


test_cf <- function(cf, df_test) {

  n_test <- length(df_test$tau)

  fit <- predict(cf, df_test$X, estimate.variance = TRUE)

  df_fit <- data_frame(point = 1:n_test,
                       tau = df_test$tau,
                       tau_hat = fit$predictions %>% as.vector(),
                       sigma_hat = sqrt(fit$variance.estimates) %>% as.vector())

  return(df_fit)
}
