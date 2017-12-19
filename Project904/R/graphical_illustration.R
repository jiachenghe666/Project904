#' @title Visualize the replication
#' @description This function is used to generate the plot in the "An Example" Section of the slide
#' @author Jiacheng He
#'
#' @import grf
#' @import ggplot2
#' @import dplyr
#' @export
#'
#'
#'
#'


graphical_illustration <- function(DGP, d, n_train, n_test) {

  df_train <- generate_true_model(DGP, d, n_train)
  df_test <- generate_true_model(DGP, d, n_test)

  cf <- causal_forest(df_train$X, df_train$Y, df_train$W)

  fit <- predict(cf, df_test$X, estimate.variance = TRUE)

  example_df <- data_frame(X1 = df_test$X[,"X1"], tau = df_test$tau,
                           tau_hat = as.vector(fit$predictions),
                           tau_ci_up = as.vector(fit$predictions + sqrt(fit$variance.estimates) * 1.96),
                           tau_ci_down = as.vector(fit$predictions - sqrt(fit$variance.estimates) * 1.96))

  example_plot <- example_df %>%
    ggplot(aes(x = X1)) +
    geom_line(aes(y = tau, color = "True tau"), size = 1.5) +
    geom_point(aes(y = tau_hat, color = "tau hat")) +
    geom_line(aes(y = tau_ci_up, color = "95% CI"), linetype = "dashed") +
    geom_line(aes(y = tau_ci_down, color = "95% CI"), linetype = "dashed") +
    ylab(expression(tau)) +
    scale_color_manual("",
                       values = c("True tau" = "red", "tau hat" = "black", "95% CI" = "black"),
                       guide = guide_legend(override.aes = list(
                         linetype = c("dashed", "blank", "solid"),
                         size = c(0.5, 1, 1.5), shape = c(NA, 16, NA))))

  return(example_plot)
}
