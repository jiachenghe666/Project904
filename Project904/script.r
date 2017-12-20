devtools::load_all()
library(ggplot2)

awesome_seed <- 904

set.seed(awesome_seed)

DGP_1 <- list(tau = 0,
              e = expression((1 + dbeta(X1, shape1 = 2, shape2 = 4)) / 4),
              m = expression(2 * X1 - 1))

DGP_2 <- list(tau = expression((1 + 1 / (1+exp(-20*(X1-1/3)))) * (1 + 1 / (1+exp(-20*(X2-1/3))))),
              e = 0.5, m = 0)

n_list <- c(100, 500, 1000, 2000, 5000)
d_list <- c(2, 4, 10, 20, 40)
s_list <- c(0.1, 0.2, 0.3, 0.4, 0.5)
t_list <- c(4, 5, 6, 7, 8)
b_list <- c(500, 1000, 2000, 4000, 6000)
size_list <- c(0, 10, 20, 40, 80)
lambda_list <- c(0, 0.1, 1, 5, 10)


output_n_DGP1 <- simulation(n_list, param_type = "n", DGP = DGP_1, bs_num = 100, file_name = "output_n_DGP1")
output_d_DGP1 <- simulation(d_list, param_type = "d", DGP = DGP_1, bs_num = 100, file_name = "output_d_DGP1")
output_n_DGP2 <- simulation(n_list, param_type = "n", DGP = DGP_2, bs_num = 100, file_name = "output_n_DGP2")
output_d_DGP2 <- simulation(d_list, param_type = "d", DGP = DGP_2, bs_num = 100, file_name = "output_d_DGP2")

output_s <- simulation(s_list, param_type = "sample.fraction", DGP = DGP_2, bs_num = 100, file_name = "output_s")
output_t <- simulation(t_list, param_type = "mtry", DGP = DGP_2, bs_num = 100, file_name = "output_t")
output_b <- simulation(b_list, param_type = "num.trees", DGP = DGP_2, bs_num = 100, file_name = "output_b")
output_size <- simulation(size_list, param_type = "min.node.size", DGP = DGP_2, bs_num = 100, file_name = "output_size")
output_lambda <- simulation(lambda_list, param_type = "lambda", DGP = DGP_2, bs_num = 100, file_name = "output_lambda")



