rm(list = ls())
deps1 <- lapply(X   = list.files(path = "./activation_helpers"),
                FUN = function(x){source(paste0("./activation_helpers/", x))})
deps2 <- lapply(X   = list.files(path = "./nn_helpers"),
                FUN = function(x){source(paste0("./nn_helpers/", x))})
deps3 <- lapply(X   = list.files(path = "./simple_helpers"),
                FUN = function(x){source(paste0("./simple_helpers/", x))})

rm(deps1, deps2, deps3)


# x <- matrix(rnorm(n = 2 * 500), nrow = 2, ncol = 500, byrow = T)
# y <- matrix(round(rnorm(n = 1 * 500, 0.5, 0.1), 0), 1, 500, byrow = T)
x <- matrix(c(0.15, 0.20, 0.11, 0.28), nrow = 2, ncol = 2)
y <- matrix(c(0, 1), nrow = 1, ncol = 2)


layers1 <- get_layer_size(x, y, hn = c(4, 2))
set.seed(123)
init_p1 <- init_params(layers1)

fwd_prop1 <- fwd_propagation(x, init_p1, c("relu", "relu","relu"), layers1, 0.05)
bck_prop1 <- backward_propagation(x, y, fwd_prop1, init_p1, layers1)


layers2 <- get_ls_simple(x, y, hn = 4)
set.seed(123)
init_p2 <- init_simple(x, layers2)

fwd_prop2 <- fwd_simple(x, init_p2, layers2)
bck_prop2 <- back_simple(x, y, fwd_prop2, init_p2, layers2)

identical(names(fwd_prop1), names(fwd_prop2))

lapply(X = dim(fwd_prop1[[1]]),
       FUN = function(d){
         as.character(
           lapply(X   = names(fwd_prop1), 
                  FUN = function(i){identical(dim(fwd_prop1[[i]])[d], 
                                              dim(fwd_prop2[[i]])[d])}))
       })

dim(bck_prop1[['db2']])
dim(bck_prop2[['db2']])




