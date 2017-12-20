#' @title The function accepts the key parameters to execute everything
#' @description On the final script (which might be submitted to the cluster) we only run this function
#' @author Jiacheng He
#' @export
#' @import parallel
#' @import purrr
#' @import dplyr
#'
#' @param param_list The values of the parameter we want to vary
#' @param param_type The name of the parameter in the causal_forest()
#' @param DGP The list of data generating process
#' @param bs_num How many times of replication for each senario
#' @param n_test Sample size of the test set
#' @param file_name The name of the file we want to the save the results in
#'
#' @return A data frame containing the MSE and coverage rate for each simulation under different parameter
#' settings
#' @examples
#' result_n_DGP1 <- simulation(param_list = n_list, param_type = "n", file_name = "output_n_DGP1")


simulation <- function(param_list, param_type, DGP, bs_num=100, n_test=100, evaluate=FALSE, file_name=NULL) {

  if (param_type != "d"){
    df_test <- generate_true_model(DGP, d = 10, n = n_test)
  }

  core_num <- detectCores()
  output_list <- list()
  i <- 1

  for (param in param_list) {

    if (param_type == "d") {
      df_test <- generate_true_model(DGP, d = param, n = n_test)
    }

    fit_list <- mclapply(1:bs_num, function(b){
      train_cf(DGP, param, param_type) %>%
        test_cf(df_test) %>%
        mutate(rep = b)
      }, mc.cores = core_num)

    fit <- reduce(fit_list, bind_rows)

    fit$param <- param

    output_list[[i]] <- fit
    i <- i + 1

    rm(fit_list)
    rm(fit)
  }

  final_output <- reduce(output_list, bind_rows)

  if (evaluate) {
    final_output <- evaluate_cf(final_output)
  }

  if (!is.null(file_name)) {save(final_output, file = paste0(file_name, ".RData"))}

  return(final_output)
}
