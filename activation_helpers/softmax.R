#' Softmax activation function
#' 
#' Returns softmax activation function.
#'
#' @param x numeric vector of outputs
#'
#' @return numeric vector 
#' @export
#'
#' @examples
#' x <- c(1, 4, 3, 6)
#' softmax(x)
softmax <- function(x){return(exp(x) / sum(exp(x)))}



