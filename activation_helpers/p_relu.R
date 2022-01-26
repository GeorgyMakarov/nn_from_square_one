#' Parametric ReLU
#' 
#' Returns parametric ReLU. The coefficient of leakage is a parameter, that is
#' learned along with the other neural-network parameters.
#' 
#' Pros:
#' 
#' - allows negative slope  
#' 
#' Cons:
#' 
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
#' p_relu(0.5, -5) == -2.5
#' p_relu(0.5, 5)  == 5
p_relu <- function(alpha, x){
  out <- x
  out[out < 0] <- alpha * out[out < 0]
  return(out)
}



