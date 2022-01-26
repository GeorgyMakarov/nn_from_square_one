#' Leaky ReLu activation function
#' 
#' Returns array value if `x > 0` and `0.01` of arrary value if `x < 0`.
#' 
#' Pros:
#' 
#' - prevents dying relu problem
#' 
#' Cons:
#' 
#' - inconsistent for negative inputs  
#' - works bad with high learning rate  
#'
#' @param x numeric vector of outputs
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' leaky_relu(5) == 5
#' leaky_relu(-5) == -0.05
leaky_relu <- function(x){
  out <- x
  out[out < 0] <- 0.01 * out[out < 0]
  return(out)
}
