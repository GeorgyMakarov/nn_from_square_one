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


grad_descent <- function(w      = NULL, 
                         b      = NULL, 
                         x_df   = NULL, 
                         y_vect = NULL, 
                         n_iter = NULL, 
                         lr     = NULL){
  
  costs <- as.numeric()
  
  for (i in 1:n_iter){
    
    prop_res <- propagate(w, b, x_df, y_vect)
    dw <- prop_res$dw
    db <- prop_res$db
    
    w <- w - lr * dw
    b <- b - lr * db
    
    if (i %% 100 == 0){
      costs <- c(costs, prop_res$cost)
    }
  }
  
  params <- list("w" = w, "b" = b)
  grads  <- list("dw" = dw, "db" = db)
  out    <- list(params, grads)
  
  return(out)
  
}

grad_descent(w = w_test, b = b_test, x_df = x_test, y_vect = y_test, n_iter = 100, lr = 0.009)



w_test <- matrix(1:2, nrow = 2, ncol = 1)
b_test <- 2
x_test <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
y_test <- c(1, 0)
propagate(w = w_test, b = b_test, x_df = x_test, y_vect = y_test)
