rm(list = ls())
deps1 <- lapply(X   = list.files(path = "./activation_helpers"),
                FUN = function(x){source(paste0("./activation_helpers/", x))})
deps2 <- lapply(X   = list.files(path = "./nn_helpers"),
                FUN = function(x){source(paste0("./nn_helpers/", x))})
# deps3 <- lapply(X   = list.files(path = "./simple_helpers"),
#                 FUN = function(x){source(paste0("./simple_helpers/", x))})
# 
# rm(deps1, deps2, deps3)
rm(deps1, deps2)


x <- matrix(c(0.15, 0.11, 0.12, 0.20, 0.28, 0.30), nrow = 2, ncol = 3, byrow = T)
y <- matrix(c(0, 1, 1), nrow = 1, ncol = 3)

l <- get_layer_size(x, y, hn = c(5, 2))

set.seed(123)
p <- init_params(l)

fwd_prop <- forward_propagation(x      = x, 
                                params = p, 
                                actif  = c("relu", "relu", "sigmoid"),
                                layers = l)




