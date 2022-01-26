deps1 <- lapply(X   = list.files(path = "./activation_helpers"),
                FUN = function(x){source(paste0("./activation_helpers/", x))})
deps2 <- lapply(X   = list.files(path = "./nn_helpers"),
                FUN = function(x){source(paste0("./nn_helpers/", x))})

rm(deps1, deps2)


x      <- matrix(rnorm(n = 2 * 500), nrow = 2, ncol = 500, byrow = T)
y      <- matrix(rnorm(n = 1 * 500), nrow = 1, ncol = 500, byrow = T)
layers <- get_layer_size(x, y, hn = c(5, 4))
set.seed(123)
init_p   <- init_params(layers)
fwd_prop <- fwd_propagation(x     = x, 
                            par   = init_p, 
                            arch  = c("tanh","relu", "sigmoid"), 
                            ls    = layers,
                            alpha = 0.05)
names(fwd_prop)

