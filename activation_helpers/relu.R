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
  a        <- x
  a[a < 0] <- 0
  ch1      <- dim(x)[1] == dim(a)[1] && dim(x)[2] == dim(a)[2]
  if (!ch1){stop("Dimensions of activation and input do not match")}
  out      <- list("a" = a, "z" = x)
  return(out)
}
