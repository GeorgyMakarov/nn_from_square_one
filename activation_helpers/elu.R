#' Exponential linear unit activation
#' 
#' Returns exponential linear unit activation function. The coefficient of 
#' leakage is a parameter, that is learned along with the other neural-network 
#' parameters.
#' 
#' Pros:
#' 
#' - solves dying ReLU problem  
#' - zero-centered
#' 
#' Cons:
#' 
#' - computes slower than other units  
#' - inconsistent for negative inputs  
#' - works bad with high learning rate  
#'
#' @param alpha coefficient of leakage
#' @param x numeric vector of outputs
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' elu(0.1, 5) == 5
#' round(elu(0.1, -2), 2) == -0.09
elu <- function(alpha = 0.01, x){
  out          <- x
  out[out < 0] <- alpha * (exp(out[out < 0]) - 1)
  return(out)
}

