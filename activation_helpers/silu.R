#' Swish activation function
#' 
#' This is a smoother version of ReLU.
#'
#' @param x numeric vector of outputs
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' round(silu(5), 2)  == 4.97
#' round(silu(-5), 2) == -0.03
silu <- function(x){return(x * sigmoid(x))}



