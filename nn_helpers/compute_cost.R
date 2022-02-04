#' Compute cost
#' 
#' Computes the cost of forward propagation results.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param cache list of forward propagation results
#' @param l list of neural network layers
#'
#' @return numeric scalar
#' @export
compute_cost <- function(x, y, cache, l){
  
  # Define the last layer activation. Do it programmatically because there is
  # no information on how many layers there are.
  last_a <- length(l) - 1 ## always l - 1 number of layers
  last_a <- paste0('a', last_a)
  
  # Check if defined last activation is in function input
  if (!last_a %in% names(cache)){stop("Activation and layers do not match")}
  
  # Compute cost
  m <- dim(x)[2]
  a <- cache[[last_a]]
  
  log_p <- (log(a) * y) + (log(1 - a) * (1 - y))
  cost  <- - (1 / m) * sum(log_p)
  return(cost)
}
