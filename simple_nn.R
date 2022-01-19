# Make binary classifier

pkgs <- c("dplyr")
deps <- lapply(pkgs, library, character.only = T)
data("iris")
source("logistic_regression_nn.R")
rm(pkgs, deps)


# Prepare initial data ---------------------------------------------------------


# We use a subset of `iris` dataset for training and testing. Versicolor and
# virginica intersect non-linearly by sepal length and sepal width.
plot(x     = iris$Sepal.Length[iris$Species == "virginica"],
     y     = iris$Sepal.Width[iris$Species == "virginica"],
     col   = "dodgerblue1",
     pch   = 19,
     frame = F,
     xlab  = "x1",
     ylab  = "x2")
points(x   = iris$Sepal.Length[iris$Species == "versicolor"],
       y   = iris$Sepal.Width[iris$Species == "versicolor"],
       col = "green",
       pch = 19)

mydata <- 
  iris %>% 
  filter(Species %in% c("virginica", "versicolor")) %>% 
  select(c("Sepal.Length", "Sepal.Width", "Species"))

rm(iris)


# Shuffle the dataset, so that different species are mixed to avoid bias from
# ordered initial data. Randomly split the dataset into training and testing.
set.seed(123)
shuffle  <- sample(x = 1:nrow(mydata), size = nrow(mydata), replace = F)
mydata <- mydata[shuffle, ]
rm(shuffle)

mydata$y <- 0
mydata$y[mydata$Species == "versicolor"] <- 1
mydata           <- mydata %>% select(-Species)
colnames(mydata) <- c("x1", "x2", "y")

set.seed(123)
in_train <- sample(x = 1:nrow(mydata), size = 0.8 * nrow(mydata), replace = F)
train_set <- mydata[in_train, ]
test_set  <- mydata[-in_train, ]
rm(in_train)


# Build simple feed-forward neural network -------------------------------------


# Define neural network architecture
# The number of nodes in hidden layer must be mean of the number of input nodes
# and the number of output nodes
# Activation function of 1st layer -- tanh
# Activation function of 2nd layer -- sigmoid
# Cut-off class probability        -- 0.5
n_x <- 2 ## input neurons, equivalent to number of feature variables
n_h <- 4 ## the size of the hidden layer -- arbitrary here, for demo purposes
n_y <- 1 ## single output neuron, showing probability of class 1


# Preprocess the data
# Standardize the features variables
# Do not change the output variable
x_train <- scale(train_set[, c(1:2)])
y_train <- train_set$y
x_test  <- scale(test_set[, c(1:2)])
y_test  <- test_set$y


# Add another dimension to output variable to convert it to matrix
dim(y_train) <- c(length(y_train), 1)
dim(y_test)  <- c(length(y_test), 1)


# Transpose matrices for better multiplication
x_train <- t(x_train)
x_test  <- t(x_test)
y_train <- t(y_train)
y_test  <- t(y_test)


# Get layer sizes
# We can avoid hard coding the network architecture if we pick it from input
# data. Shapes of input matrices can tell us the number of nodes for input
# and output layers. We define a custom function to do that.


#' Get layer sizes
#' 
#' This function computes layer sizes of neural network. We need these sizes to
#' be able to create the architecture of the neural network.
#' 
#' The function computes the size of hidden layer as the nearest integer derived
#' from the mean of the sizes of input and output layers. There is an alternative
#' to provide the size of the hidden layer manually.
#' 
#' This function computes the size of one hidden layer.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param hn number of hidden layer nodes
#'
#' @return list
#' @export
get_layer_size <- function(x, y, hn = NULL){
  
  n_x <- dim(x)[1]
  n_y <- dim(y)[1]
  
  ifelse(test = is.null(hn),
         yes  = n_h <- round(mean(x = c(n_x, n_y)), 0),
         no   = n_h <- hn)
  
  out <- list("n_x" = n_x, "n_h" = n_h, "n_y" = n_y)
  return(out)
}


layer_size <- get_layer_size(x_train, y_train, hn = 4)


# Initialize parameters --------------------------------------------------------


# There are two ways to initialize parameters: from zero, from random uniform
# distribution. We will implement both and see, which is faster and more
# accurate. We have two sets of parameters, because we have 3 layers -- and
# parameters apply between layers. Parameters are matrices, which depend on the
# sizes of different layers.
# The sizes of weight matrices:
# w1 = (n_h, n_x)
# b1 = (n_h, 1)
# w2 = (n_y, n_h)
# b2 = (n_y, 1)


#' Initialize parameters from random uniform distribution
#' 
#' This function initializes parameters of neural network from a random uniform
#' distribution.
#'
#' @param x matrix of feature variables
#' @param l_size list of layer sizes
#'
#' @return list of weight matrices
#' @export
init_params_unif <- function(x, l_size){
  
  m   <- dim(x)[2]
  n_x <- l_size[['n_x']]
  n_h <- l_size[['n_h']]
  n_y <- l_size[['n_y']]
  
  w1  <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = T) * 0.01
  b1  <- matrix(rep(0, n_h), nrow = n_h)
  w2  <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = T) * 0.01
  b2  <- matrix(rep(0, n_y), nrow = n_y)
  
  params <- list("w1" = w1, "b1" = b1, "w2" = w2, "b2" = b2)
  return(params)
}


init_pu <- init_params_unif(x = x_train, l_size = layer_size)


# Forward propagation ----------------------------------------------------------


#' Forward propagation
#' 
#' Computes forward propagation for neural network
#'
#' @param x matrix of feature variables
#' @param param list of parameters
#' @param l_size list of layers sizes
#'
#' @return list of updated coefficients
#' @export
forward_prop <- function(x, param, l_size){
  
  m   <- dim(x)[2]
  n_h <- l_size[['n_h']]
  n_y <- l_size[['n_y']]
  
  w1 <- param[['w1']]
  b1 <- param[['b1']]
  w2 <- param[['w2']]
  b2 <- param[['b2']]
  
  b1_adj <- matrix(rep(b1, m), nrow = n_h)
  b2_adj <- matrix(rep(b2, m), nrow = n_y)
  
  z1 <- w1 %*% x + b1_adj
  a1 <- tanh(z1)
  z2 <- w2 %*% a1 + b2_adj
  a2 <- sigmoid(z2)
  
  out <- list("z1" = z1, "a1" = a1, "z2" = z2, "a2" = a2)
  return(out)
}

fwd_prop_pu <- forward_prop(x = x_train, param = init_pu, l_size = layer_size)


# Compute cost -----------------------------------------------------------------

#' Compute cost
#' 
#' Computes the cost of forward propagation results.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param cache list of forward propagation results
#'
#' @return numeric scalar
#' @export
compute_cost <- function(x, y, cache){
  
  m  <- dim(x)[2]
  a2 <- cache[['a2']]
  
  log_p <- (log(a2) * y) + (log(1 - a2) * (1 - y))
  cost  <- - (1 / m) * sum(log_p)
  return(cost)
}

cost_pu <- compute_cost(x = x_train, y = y_train, cache = fwd_prop_pu)


# Backpropagation --------------------------------------------------------------

# Backpropagation accepts input matrix, output variable matrix, cached forward
# propagation results, list of layer sizes.

#' Back propagation
#' 
#' Computes back propagation for neural net.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param cache list of forward propagation results
#' @param param list of initial parameters
#' @param l_size list of layers sizes
#'
#' @return list of derivatives
#' @export
back_prop <- function(x, y, cache, param, l_size){
  
  m   <- dim(x)[2]
  n_x <- l_size[['n_x']]
  n_h <- l_size[['n_h']]
  n_y <- l_size[['n_y']]
  a2  <- cache[['a2']]
  a1  <- cache[['a1']]
  w2  <- param[['w2']]
  
  dz2     <- a2 - y
  dw2     <- (1/m) * (dz2 %*% t(a1))
  db2     <- matrix((1/m) * sum(dz2), nrow = n_y)
  db2_adj <- matrix(rep(db2, m), nrow = n_y)
  
  dz1     <- (t(w2) %*% dz2) * (1 - a1 ^ 2)
  dw1     <- (1 / m) * (dz1 %*% t(x))
  db1     <- matrix((1 / m) * sum(dz1), nrow = n_h)
  db1_adj <- matrix(rep(db1, m), nrow = n_h)
  
  out <- list("dw1" = dw1, "db1" = db1, "dw2" = dw2, "db2" = db2)
  return(out)
}


bck_pu <- back_prop(x_train, y_train, fwd_prop_pu, init_pu, layer_size)


# Update parameters after back propagation computation

update_param <- function(grad, param, lr){
  
  w1 <- param[['w1']]
  b1 <- param[['b1']]
  w2 <- param[['w2']]
  b2 <- param[['b2']]
  
  dw1 <- grad[['dw1']]
  db1 <- grad[['db1']]
  dw2 <- grad[['dw2']]
  db2 <- grad[['db2']]
  
  # Update initial parameters using learning rate
  w1 <- w1 - lr * dw1
  b1 <- b1 - lr * db1
  w2 <- w2 - lr * dw2
  b2 <- b2 - lr * db2
  
  out <- list("w1" = w1, "b1" = b1, "w2" = w2, "b2" = b2)
  return(out)
}


upd_pu <- update_param(bck_pu, init_pu, lr = 0.01)
rm(bck_pu, fwd_prop_pu, init_pu, layer_size, upd_pu, cost_pu, n_h, n_x, n_y)


# Assemble neural net model ----------------------------------------------------

# Sequence of model computations:
# 1. forward propagation
# 2. calculate loss
# 3. update parameters
# 4. go to 1.

#' Train neural network
#' 
#' Trains simple one-layer feed-forward neural network.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param epochs numeric scalar, number of iterations
#' @param hn numeric scalar, number of hidden layer nodes
#' @param lr numeric scalar, learning rate
#' @param trac logical, track progress if TRUE
#'
#' @return list
#' @export
train_nn <- function(x, y, epochs, hn, lr, trac = F){
  
  l_size <- get_layer_size(x = x, y = y, hn = hn)
  init_p <- init_params_unif(x = x, l_size = l_size)
  cost_h <- c() ## variable to store cost
  trackr <- 0.1 * epochs
  
  for (i in 1:epochs){
    
    fwd_prop <- forward_prop(x= x, param = init_p, l_size = l_size)
    cost     <- compute_cost(x = x, y = y, cache = fwd_prop)
    bck_prop <- back_prop(x, y, fwd_prop, init_p, l_size)
    upd_par  <- update_param(grad = bck_prop, param = init_p, lr = lr)
    init_p   <- upd_par
    cost_h   <- c(cost_h, cost)
    
    # Trace output -- track 10% of computations
    if (i %% trackr == 0){cat("Iteration", i, " | Cost: ", cost, "\n")}
  }
  
  model_out <- list("upd_params" = upd_par, "cost_history" = cost_h, "hn" = hn)
  return(model_out)
}


fit <- train_nn(x_train, y_train, epochs = 10000, hn = 10, lr = 0.9, trac = T)
plot(fit$cost_history, type = "l", frame = F, xlab = "", ylab = "cost")


# Predict with neural network --------------------------------------------------

#' Predict neural network
#' 
#' Predicts output variable using neural network model
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param md model file
#' @param conv logical, convert probabilities to binary if TRUE
#' @param cut_off cut off value for probabilities
#'
#' @return list
#' @export
predict_nn <- function(x, y, md, conv = F, cut_off){
  hn       <- md[['hn']]
  l_size   <- get_layer_size(x, y, hn)
  params   <- md[['upd_params']]
  fwd_prop <- forward_prop(x, params, l_size)
  out      <- fwd_prop[['a2']]
  
  # Convert probabilities to binary values based on cut_off
  if (conv){out <- ifelse(test = out >= cut_off, yes = 1, no = 0)}
  return(out)
}

pred_fit <- predict_nn(x_test, y_test, md = fit, cut_off = 0.5, conv = T)


# Compute metrics ---------------------------------------------------------

#' Compute accuracy metrics
#' 
#' Computes a set of accuracy metrics for classifier.
#'
#' @param y_test matrix of actual output values
#' @param y_hat matrix of predicted output values
#' @param md_name character, model name
#' @param show print metrics
#'
#' @return list
#' @export
compute_metrics <- function(y_test, y_hat, md_name, show = T){
  tb   <- table(y_test, y_hat)
  acc  <- round((tb[1] + tb[4]) / sum(tb), 2)
  rec  <- round(tb[4] / (tb[4] + tb[3]), 2)
  pre  <- round(tb[4] / (tb[4] + tb[2]), 2)
  f1sc <- round(2 * ((pre * rec) / (pre + rec)), 2)
  res  <- list("accuracy" = acc, "precision" = pre, "recall" = rec, "f1" = f1sc)
  
  res_p <- paste0(md_name, ": \n",
                  paste0("\tAccuracy = ", acc * 100, "% \n"),
                  paste0("\tPrecision = ", pre * 100, "% \n"),
                  paste0("\tRecall = ", rec * 100, "% \n"),
                  paste0("\tF1 score = ", f1sc * 100, "% \n"))
  if (show){cat(res_p)}
  return(res)
}


compute_metrics(y_test, pred_fit, md_name = "neural network", show = T)