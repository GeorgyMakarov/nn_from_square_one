#' Activation backward
#' 
#' This function computes derivative of activation function. Derivative is a
#' part of backpropagation module.
#'
#' @param da derivative of activation
#' @param z linear module
#' @param f activation function
#'
#' @return matrix
#' @export
activation_backward <- function(da, z, f){
  f <- paste0(f, "_backward")
  f <- paste0(f, "(z)")
  g <- eval(parse(text = f))
  res <- da * g
  return(res)
}