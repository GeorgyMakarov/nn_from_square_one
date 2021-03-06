#' Update parameters
#' 
#' Update parameters of neural network with gradients.
#'
#' @param bp list, back propagation
#' @param p list, parameters
#' @param lr numeric scalar, learning rate
#' @param ll numeric vector, size of layers
#'
#' @return list of matrices
#' @export
update_params <- function(bp, p, lr, ll){
  res <- list() ## define list for output
  for (i in 1:(length(ll) - 1)){
    
    # Assign temporary variables
    w  <- paste0('w', i)
    b  <- paste0('b', i)
    dw <- paste0('dw', i)
    db <- paste0('db', i)
    
    # Adjust parameters with learning rate
    res[[w]] <- p[[w]] - lr * bp[[dw]]
    res[[b]] <- p[[b]] - lr * bp[[db]]
  }
  return(res)
}