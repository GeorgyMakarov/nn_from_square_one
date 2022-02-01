rm(list = ls())
# deps1 <- lapply(X   = list.files(path = "./activation_helpers"),
#                 FUN = function(x){source(paste0("./activation_helpers/", x))})
# deps2 <- lapply(X   = list.files(path = "./nn_helpers"),
#                 FUN = function(x){source(paste0("./nn_helpers/", x))})
# deps3 <- lapply(X   = list.files(path = "./simple_helpers"),
#                 FUN = function(x){source(paste0("./simple_helpers/", x))})
# 
# rm(deps1, deps2, deps3)

source("./nn_helpers/get_layer_size.R")
source("./nn_helpers/init_params.R")
source("./nn_helpers/linear_fwd.R")
source("./nn_helpers/linear_act_fwd.R")
source("./activation_helpers/sigmoid.R")
source("./activation_helpers/relu.R")


x <- matrix(c(0.15, 0.11, 0.12, 0.20, 0.28, 0.30), nrow = 2, ncol = 3, byrow = T)
y <- matrix(c(0, 1, 1), nrow = 1, ncol = 3)

# l <- get_layer_size(x, y, hn = c(5, 2))
# 
# set.seed(123)
# p <- init_params(l)
# lapply(X = p, FUN = function(i){dim(i)})
# 
# lin_fwd <- linear_fwd(x, p[['w1']], p[['b1']])


# Data for manual testing
w1 <- seq(0.1, 1.0, by = 0.1)
w1 <- matrix(w1, nrow = 5, ncol = 2, byrow = T)
b1 <- seq(0.1, 0.5, by = 0.1)
b1 <- matrix(b1, nrow = 5, ncol = 1)

lin_fwd <- linear_fwd(x, w1, b1)
lin_act <- linear_act_fwd(x, w1, b1, "sigmoid")
lin_act <- linear_act_fwd(x, w1, b1, "relu")






