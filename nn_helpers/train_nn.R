#' Train neural network
#' 
#' Trains feed forward neural network. This function uses gradient descent or
#' Adam optimization algorithm. User can select different network architectures
#' and tune hyper parameters.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param epochs numeric scalar, number of iterations
#' @param hn numeric scalar, number of hidden layer nodes
#' @param lr numeric scalar, learning rate
#' @param f character vector, names of activation functions
#' @param im character scalar, name of initialization method: stand, rand, he
#' @param lambda
#' @param kp
#' @param track logical, track progress if TRUE
#' @param optim character scalar, optimization algorithm: gradient descent, Adam
#' @param beta1 numeric scalar, coefficient for Adam algorithm
#' @param beta2 numeric scalar, coefficient for Adam algorithm
#' @param epsilon numeric scalar, coefficient for Adam algorithm
#' @param b_size numeric scalar, mini batch size
#'
#' @return list
#' @export
train_nn <- function(x, 
                     y, 
                     epochs, 
                     lr, 
                     hn, 
                     f, 
                     im = c("stand", "rand", "he"),
                     lambda = 0,
                     kp     = 1,
                     track  = F,
                     optim  = c("gd", "adam"),
                     beta1  = 0.9,
                     beta2  = 0.999,
                     epsil  = 1e-08,
                     b_size = 64){
  if (is.null(optim)){optim <- "gd"}
  if (length(optim) == 2){optim <- "gd"}
  if (optim == "gd"){
    res <- train_nn_standard(x, y, epochs, lr, hn, f, im, lambda, kp, track)
  } else {
    res <- train_nn_adam(x, y, epochs, lr, hn, f, im, lambda, kp, track, beta1, 
                         beta2, epsil, b_size)
  }
  return(res)
}
