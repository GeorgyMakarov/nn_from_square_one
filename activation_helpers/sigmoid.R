#' Compute sigmoid activation function
#' 
#' Computes sigmoid activation function for a given linear output
#'
#' @param z a numeric vector of outputs for `z = w.t * x + b`
#'
#' @return numeric vector
#' @export
#'
sigmoid <- function(x){
  a     <- 1 / (1 + exp(-x))
  out   <- list("a" = a, "z" = x)
  return(out)
}
