box_plot_final_output <- function(final_output, x_lab=NULL, title=NULL, y_scale_MSE=NULL, y_scale_coverage=NULL, y_scale_sigma=NULL) {

  evaluation <- evaluate_cf(final_output)

  MSE <- evaluation$MSE %>%
    ggplot() +
    geom_boxplot(aes(factor(param), MSE)) +
    scale_y_continuous(limit = y_scale_MSE) +
    labs(x = x_lab, title = title)

  coverage <- evaluation$coverage %>%
    ggplot() +
    geom_boxplot(aes(factor(param), coverage)) +
    scale_y_continuous(limit = y_scale_coverage) +
    labs(x = x_lab, y = "Coverage Rate", title = title)

  se <- final_output %>%
    ggplot() +
    geom_boxplot(aes(factor(param), sigma_hat)) +
    scale_y_continuous(limit = y_scale_sigma) +
    labs(x = x_lab, y = "Standard Error", title = title)

  return(list(MSE = MSE, coverage = coverage, se = se))
}
