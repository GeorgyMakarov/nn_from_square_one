# This is the simplest neural network classifier implementation. The motivation
# is to build a logistic regression model, which will be able to classify.


# Build neural network:
#   1. define model structure -- such as number of features
#   2. initialize model parameters
#   3. loop:
#       3.1. compute current loss -- forward propagation
#       3.2. compute current grad -- backward propagation
#       3.3. update parameters    -- gradient descent


#' Compute sigmoid activation function
#' 
#' Computes sigmoid activation function for a given linear output
#'
#' @param x a numeric vector of outputs for `z = w.t * x + b`
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' print(paste("sigmoid(0) =", sigmoid(0)))
#' print(paste("sigmoid(9.2) =", sigmoid(9.2)))
sigmoid <- function(x){return(1 / (1 + exp(-x)))}


#' Initialize parameters with zeros
#' 
#' Initialize parameters `w` and `b` with zeros. The output of the function has
#' the shapes required for matrix multiplication. The shape of parameter `w` is
#' (nx, 1) where `nx` is the number of features. The parameter `b` is a scalar
#' of length **1** as we can add it to the matrix.
#'
#' @param dim number of features in the data set, also number of inputs
#'
#' @return list of parameters `w` and `b`
#' @export
#'
#' @examples
#' initialize_with_zeros(10)
initialize_with_zeros <- function(dim){
  
  # Initialize weights with 0. The shape of the weights matrix must be (m, 1)
  # where m is the number of observations
  w   <- as.matrix(rep(0, dim))
  b   <- 0
  res <- list(w, b)
  names(res) <- c("w", "b")
  
  # Check that the shapes of parameters correspond to required shapes. This
  # check is required to avoid wrong matrix multiplication later.
  check1 <- dim(w)[1] == dim
  check2 <- class(b) == 'numeric'
  ifelse(test = check1 & check2, yes = return(res), no = return(NA))
}


#' Compute forward and backward propagation
#' 
#' Implement cost function and its gradient for the forward and backward
#' propagation.
#'
#' @param w weights, a matrix of dims `(n_x, 1)`, n_x -- number of features
#' @param b bias, a scalar
#' @param x_df a data frame with features in columns and observations in rows
#' @param y_vect a numeric vector of output classes: 1, 0
#'
#' @return a list of gradients and cost
#' @export
#'
#' @examples
#' w_test <- matrix(1:2, nrow = 2, ncol = 1)
#' b_test <- 2
#' x_test <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
#' y_test <- c(1, 0)
#' propagate(w = w_test, b = b_test, x_df = x_test, y_vect = y_test)
propagate <- function(w      = NULL, 
                      b      = NULL, 
                      x_df   = NULL, 
                      y_vect = NULL){
  
  m <- dim(x_df)[1]
  
  # Data X must be matrix of shape (n_x, m), where n_x -- the number of features
  # and m -- the number of observations. This is required for matrix dot product
  # Input comes in format (m, n_x) -- hence the transpose is required.
  xm <- t(x_df)
  wt <- t(w)
  
  # Compute `Z = w_t * X + b` -- this is a linear input to activation function
  wt_by_x  <- wt %*% xm
  z_out    <- wt_by_x + b
  
  # Compute activation function for forward propagation
  # Compute cost for the activation function
  a_forward <- sigmoid(z_out)
  log_part  <- y_vect * log(a_forward) + (1 - y_vect) * log(1 - a_forward)
  cost      <- (-1 / m) * (sum(log_part))
  
  # Run backpropagation to find gradients
  dw <- (1 / m) * (xm %*% t(a_forward - y_vect))
  db <- (1 / m) * (sum(a_forward - y_vect))
  
  # Check the shapes of input and output to ensure the computations are correct
  check1 <- identical(dim(w), dim(dw))
  check2 <- is.numeric(db)
  
  # Prepare named output list
  grads <- list("dw" = dw, "db" = db)
  out   <- list("grads" = grads, "cost" = cost)
  
  ifelse(test = check1 & check2, yes = return(out), no = return(0))
}


#' Compute gradient descent
#' 
#' Optimize model parameters by running a gradient descent algorithm.
#'
#' @param w weights, a matrix of dims `(n_x, 1)`, n_x -- number of features
#' @param b bias, a scalar
#' @param x_df a data frame with features in columns and observations in rows
#' @param y_vect a numeric vector of output classes: 1, 0
#' @param n_iter number of iterations, a scalar
#' @param lr learning rate, a scalar
#'
#' @return list of output parameters
#' @export
#'
#' @examples
#' w_test <- matrix(1:2, nrow = 2, ncol = 1)
#' b_test <- 2
#' x_test <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
#' y_test <- c(1, 0)
#' grad_descent(w      = w_test, 
#'              b      = b_test, 
#'              x_df   = x_test, 
#'              y_vect = y_test, 
#'              n_iter = 100, 
#'              lr     = 0.009)
grad_descent <- function(w      = NULL, 
                         b      = NULL, 
                         x_df   = NULL, 
                         y_vect = NULL, 
                         n_iter = NULL, 
                         lr     = NULL){
  
  costs <- as.numeric()
  
  for (i in 1:n_iter){
    
    prop_res <- propagate(w, b, x_df, y_vect)
    dw <- prop_res$grads$dw
    db <- prop_res$grads$db
    
    w <- w - lr * dw
    b <- b - lr * db
    
    if (i %% 100 == 0){
      costs <- c(costs, prop_res$cost)
    }
  }
  
  params <- list("w" = w, "b" = b)
  grads  <- list("dw" = dw, "db" = db)
  out    <- list("params" = params, "grads" = grads)
  
  return(out)
  
}


#' Compute class predictions
#' 
#' Computes class predictions. Predicts positive class if probability is more
#' than 0.5, otherwise -- predicts negative class. Positive - 1, negative - 0.
#' Use weights and bias results after gradient descent.
#'
#' @param w weights, a matrix of dims `(n_x, 1)`, n_x -- number of features
#' @param b bias, a scalar
#' @param x_df a data frame with features in columns and observations in rows
#'
#' @return numeric vector of classes [0, 1]
#' @export
#'
#' @examples
#' w_test <- matrix(1:2, nrow = 2, ncol = 1)
#' b_test <- 2
#' x_test <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
#' y_test <- c(1, 0)
#' pred_logit(w = w_test, b = b_test, x_df = x_test)
pred_logit <- function(w = NULL, b = NULL, x_df = NULL){
  
  # Compute vector a_out predicting the probabilities of class 1
  m        <- dim(x_df)[1]
  xm       <- t(x_df)
  wt       <- t(w)
  wt_by_x  <- wt %*% xm
  z_out    <- wt_by_x + b
  a_out    <- sigmoid(z_out)
  
  # Convert probabilities to actual class predictions
  y_hat <- ifelse(test = a_out > 0.5, yes = 1, no = 0)
  return(y_hat)
}


#' Logistic regression neural network
#'
#' @param x_train training data frame with variables 
#' @param y_train training vector of response variable
#' @param x_test testing data frame with variables
#' @param y_test testing vector of response variable
#' @param n_iter number of iterations, scalar
#' @param lr learning rate, scalar
#'
#' @return list of predictions and accuracy measures
#' @export
#'
#' @examples
#' x_trn <- data.frame(x1 = c(0.8, 1.1, 1.4, 1.6, 1.9, 2.1, 2.3, 2.4),
#' x2 = c(2.5, 2.7, 3.1, 3.4, 3.5, 3.7, 4.0, 4.3))
#' y_trn <- c(1, 1, 1, 1, 1, 0, 0, 0)
#' x_tst <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
#' y_tst <- c(1, 0)
#' log_nn(x_train = x_trn,
#'        y_train = y_trn,
#'        x_test  = x_tst,
#'        y_test  = y_tst,
#'        n_iter  = 2000,
#'        lr      = 0.005)
log_nn <- function(x_train = NULL,
                   y_train = NULL,
                   x_test  = NULL,
                   y_test  = NULL,
                   n_iter  = 2000,
                   lr      = 0.5){
  
  # Initialize parameters with zeros
  init_shape  <- dim(x_train)[2]
  init_params <- initialize_with_zeros(init_shape)
  init_w      <- init_params$w
  init_b      <- init_params$b
  
  # Implement gradient descent
  grad_desc   <- grad_descent(w      = init_w,
                              b      = init_b,
                              x_df   = x_train,
                              y_vect = y_train,
                              n_iter = n_iter,
                              lr     = lr) 
  
  # Retrieve updated parameters w and b
  upd_w <- grad_desc$params$w
  upd_b <- grad_desc$params$b
  
  # Predict on train and test sets
  y_hat_test  <- pred_logit(w = upd_w, b = upd_b, x_df = x_test)
  y_hat_train <- pred_logit(w = upd_w, b = upd_b, x_df = x_train)
  
  # Compute prediction errors
  train_acc <- 100 - mean(abs(y_hat_train - y_train)) * 100
  test_acc  <- 100 - mean(abs(y_hat_test - y_test)) * 100
  errors    <- list("train_acc" = train_acc,
                    "test_acc"  = test_acc)
  
  res <- list("train"    = y_hat_train, 
              "test"     = y_hat_test,
              "accuracy" = errors)
  return(res)
}