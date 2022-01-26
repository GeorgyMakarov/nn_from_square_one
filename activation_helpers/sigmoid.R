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
