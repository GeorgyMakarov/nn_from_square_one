#' Softplus activation function
#'
#' @param x numeric vector of outputs
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' softplus(5)
softplus <- function(x){return(log(exp(x) + 1))}


