#' Compute sigmoid activation function
#' 
#' Computes sigmoid activation function for a given linear output
#'
#' @param z a numeric vector of outputs for `z = w.t * x + b`
#'
#' @return numeric vector
#' @export
#'
sigmoid <- function(x){return(1 / (1 + exp(-x)))}
