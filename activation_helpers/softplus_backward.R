#' Softplus derivative
#'
#' @param z result of activation forward
#'
#' @return numeric scalar
#' @export
softplus_backward <- function(z){
  res <- 1 / (1 + exp(-z))
  return(res)
}
