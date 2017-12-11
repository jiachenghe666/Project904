library(grf); library(ggplot2); library(dplyr)

DGP <- list(tau = 0,
              e = expression((1 + dbeta(X1, shape1 = 2, shape2 = 4)) / 4),
              m = expression(2 * X1 - 1))

X <- build_covariates(d = 2, n = 400)
test_index <- sample(1:400, 200, replace = FALSE)
X_test <- X[test_index, ]
X_train <- X[-test_index, ]

if (is.list(DGP)) {
  df_train <- generate_true_model(X_train, DGP_tau = DGP$tau, DGP_e = DGP$e, DGP_m = DGP$m)
  df_test <- generate_true_model(X_test, DGP_tau = DGP$tau, DGP_e = DGP$e, DGP_m = DGP$m)
} else {
  stop("You fuck up the DGP")
}

cf <- causal_forest(df_train$X, df_train$Y, df_train$W)

fit <- predict(cf, df_test$X, estimate.variance = TRUE)

example_df <- data_frame(X1 = df_test$X[,"X1"], tau = df_test$tau,
                      tau_hat = as.vector(fit$predictions),
                      tau_ci_up = as.vector(fit$predictions + sqrt(fit$variance.estimates) * 1.96),
                      tau_ci_down = as.vector(fit$predictions - sqrt(fit$variance.estimates) * 1.96))


example_df %>%
  ggplot(aes(x = X1)) +
  geom_line(aes(y = tau, color = "red"), size = 1.5) +
  geom_point(aes(y = tau_hat)) +
  geom_line(aes(y = tau_ci_up), linetype = 2) +
  geom_line(aes(y = tau_ci_down), linetype = 2) +
  scale_color_manual()
