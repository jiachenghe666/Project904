#' @title The function accepts the key parameters to execute everything
#' @description On the final script (which might be submitted to the cluster) we only run this function
#' @author Jiacheng He
#' @export
#' @import parallel
#' @import purrr
#' @importFrom  plyr ldply
#'
#' @param param_list The values of the parameter we want to vary
#' @param param_type The name of the parameter in the causal_forest()
#' @param DGP The list of data generating process
#' @param bs_num How many times of replication for each senario
#' @param file_name The name of the file we want to the save the results in
#'
#' @return A data frame containing the MSE and coverage rate for each simulation under different parameter
#' settings
#' @examples
#' result_n_DGP1 <- simulation(param_list = n_list, param_type = "n", file_name = "output_n_DGP1")


simulation <- function(param_list, param_type, DGP, bs_num=100, file_name=NULL) {


  iterate <- function(param, param_type, DGP) {
    if (param_type == "n") {
      tmp <- replicate_sim(n_train = param, n_test = param, DGP = DGP)
    } else if (param_type == "d") {
      tmp <- replicate_sim(d = param, DGP = DGP)
    } else if (param_type == "sample.fraction") {
      tmp <- replicate_sim(sample.fraction = param, DGP = DGP)
    } else if (param_type == "mtry") {
      tmp <- replicate_sim(mtry = param, DGP = DGP)
    } else if (param_type == "num.trees") {
      tmp <- replicate_sim(num.trees = param, DGP = DGP)
    } else if (param_type == "min.node.size") {
      tmp <- replicate_sim(min.node.size = param, DGP = DGP)
    } else if (param_type == "lambda") {
      tmp <- replicate_sim(lambda = param, DGP = DGP)
    } else {stop("You fuck up the tuning parameter type")}

    output <- data_frame(MSE = tmp$MSE, coverage = tmp$coverage)
    return(output)
  }

  core_num <- detectCores()
  output_list <- list()
  i <- 1

  for (param in param_list) {
    iterate_tmp <- iterate %>% partial(param=param, param_type=param_type, DGP=DGP)
    output <- mclapply(1:bs_num, function(b){iterate_tmp()}, mc.cores = core_num) %>%
      ldply(as_data_frame)
    output$param <- param
    output_list[[i]] <- output
    i <- i + 1
  }

  final_output <- ldply(output_list, as_data_frame)

  if (!is.null(file_name)) {save(final_output, file = paste0(file_name, ".RData"))}

  return(final_output)
}
