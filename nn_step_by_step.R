rm(list = ls())
deps1 <- lapply(X   = list.files(path = "./activation_helpers"),
                FUN = function(x){source(paste0("./activation_helpers/", x))})
deps2 <- lapply(X   = list.files(path = "./nn_helpers"),
                FUN = function(x){source(paste0("./nn_helpers/", x))})

rm(deps1, deps2)


# Create input data
x <- matrix(c(0.15, 0.11, 0.12, 0.20, 0.28, 0.30), nrow = 2, ncol = 3, byrow = T)
y <- matrix(c(0, 1, 1), nrow = 1, ncol = 3)


# Compute forward propagation. -------------------------------------------------

# Forward propagation can follow any neural network architecture. Length of 
# activation functions vector must be 1 less than the length of layers.
# The computation follows the route: z = w * a + b --> a = activation(z) -->
# cost = cost(a). The output layer activation must always be sigmoid or
# softmax, as we use cross-entropy loss function.
l <- get_layer_size(x, y, hn = c(5, 2))
f <- c("relu", "relu", "sigmoid")

set.seed(123)
p <- init_params(l)

fwd_prop <- forward_propagation(x = x, params = p, actif = f, layers = l)
l_cost   <- compute_cost(x, y, fwd_prop, l, f)


# Compute backward propagation --------------------------------------------

bck_prop <- back_propagation(x, y, p, fwd_prop, l, f)





