#' ReLU activation function
#' 
#' Returns array value if `x > 0` and 0 value if `x <= 0`.
#' 
#' Pros:
#' 
#' - sparse activation -- only 50% of units are initially active  
#' - good gradient propagation -- saturation in one direction  
#' - efficient computation  
#' 
#' Cons:
#' 
#' - dying relu -- when inputs approach zero or are negative, NN can't learn  
#' - not zero centered  
#' - does not have boundaries  
#'
#' @param x numeric vector of outputs
#'
#' @return numeric vector
#' @export
relu <- function(x){
  out          <- x
  out[out < 0] <- 0
  return(out)
}
