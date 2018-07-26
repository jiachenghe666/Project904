# Monte Carlo Simulation on Causal Forest
This is a simulation project on the causal forest method by [Wager and Athey (2017)](https://arxiv.org/pdf/1510.04342.pdf). Causal forest is a mix of machine learning and causal inference. It is a random forest type method to estimate heterogenous treatment effects. The treatment effect of each individual is assumed to be a function of individual's pre-treatment covariates. 

Rather than to *predict* an outcome variable, causal forest is designed to *estimate* a parameter which captures the outcome difference between the treatment group and the control group. The asymptotic distribution theory is established thus we can obtain standard errors of the treatment effect estimates for each individuals, which makes causal forest very suitable for causal inference in the settings of randomized experiment or unconfoundedness. 

In Project904, I test the finite sample performance of causal forest under various empirical environment and tuning parameter settings.

The metrics I use include the mean square error between estimated treatment effect and true treatment effect (predictive accuracy of the point estimate), as well as the confidence interval coverage rate under repeated sampling (e.g. whether the 95% confidence interval really covers the true effect in 95% of the cases). 


## Report
Please see [here](https://github.com/JiachengHe/Project904/blob/master/paper/paper.pdf) for the short report, and see [here](https://github.com/JiachengHe/Project904/blob/master/Slide/slide.pdf) for the presentation slides.


## R package
The R package documents the workflow of the Monte Carlo simulation. I write self-explanatory functions:

modeling pipeline: ```train_cf```,  ```test_cf```, ```evaluate_cf``` (cf a.k.a causal forest) \
data generating: ```build_covariate```, ```generate_true_model``` \
visualization: ```graphical_illustration```, ```box_plot_final_output```, ```multiplot``` \
wrapper function: ```simulation``` 


## Main script
You could run 
```R CMD BATCH ./Project904/script.r```
to replicate the simulation.

With code packaging, the main script of the simulation work is concise and self-explanatory.

```R
devtools::load_all()

awesome_seed <- 904
set.seed(awesome_seed)


#### DGP: Data generating process
## tau: true treatment effect function (on observed covariates)
## e: treatment propensity function
## m: untreated mean outcome function
DGP_1 <- list(tau = 0,
              e = expression((1 + dbeta(X1, shape1 = 2, shape2 = 4)) / 4),
              m = expression(2 * X1 - 1))
DGP_2 <- list(tau = expression((1 + 1 / (1+exp(-20*(X1-1/3)))) * (1 + 1 / (1+exp(-20*(X2-1/3))))),
              e = 0.5, 
              m = 0)
              
             
#### Empirical environment 
## n: sample size
## d: number of covariates
#### Model tuning parameters
## s: sample fraction used for training each tree
## t: number of covraites used for training each tree
## b: number of trees trained
## size: minimum node size
## lambda: regularization parameter
n_list <- c(100, 500, 1000, 2000, 5000)
d_list <- c(2, 4, 10, 20, 40)
s_list <- c(0.1, 0.2, 0.3, 0.4, 0.5)
t_list <- c(4, 5, 6, 7, 8)
b_list <- c(500, 1000, 2000, 4000, 6000)
size_list <- c(0, 10, 20, 40, 80)
lambda_list <- c(0, 0.1, 1, 5, 10)


## bs_num: number of bootstrap (repeated sampling)
output_n_DGP1 <- simulation(n_list, param_type="n", DGP=DGP_1, bs_num=100, file_name="output_n_DGP1")
output_d_DGP1 <- simulation(d_list, param_type="d", DGP=DGP_1, bs_num=100, file_name="output_d_DGP1")
output_n_DGP2 <- simulation(n_list, param_type="n", DGP=DGP_2, bs_num=100, file_name="output_n_DGP2")
output_d_DGP2 <- simulation(d_list, param_type="d", DGP=DGP_2, bs_num=100, file_name="output_d_DGP2")

output_s <- simulation(s_list, param_type="sample.fraction", DGP=DGP_2, bs_num=100, file_name="output_s")
output_t <- simulation(t_list, param_type="mtry", DGP=DGP_2, bs_num=100, file_name="output_t")
output_b <- simulation(b_list, param_type="num.trees", DGP=DGP_2, bs_num=100, file_name="output_b")
output_size <- simulation(size_list, param_type="min.node.size", DGP=DGP_2, bs_num=100, file_name="output_size")
output_lambda <- simulation(lambda_list, param_type="lambda", DGP=DGP_2, bs_num=100, file_name="output_lambda")
```
