#' Forward propagation
#' 
#' Computes forward propagation by taking input matrix of features variables
#' and initial parameters.
#'
#' @param x feature matrix
#' @param params list of initial parameters
#' @param actif vector of activation functions
#' @param layers vector of layers
#'
#' @return list of z and a
#' @export
forward_propagation <- function(x, params, actif, layers){
  
  l <- length(layers) - 1
  a <- x
  
  for (i in 1:l){
    f <- actif[i]
    w <- params[[paste0('w', i)]]
    b <- params[[paste0('b', i)]]
    z <- linear_fwd(a, w, b)
    a <- activation_fwd(z, f)
    assign(x = paste0('z', i), value = z)
    assign(x = paste0('a', i), value = a)
  }
  
  # Remove all variables that we do not need in the output. This is
  # required to be able to handle as many layers as required.
  rm(x, params, actif, layers, f, w, b, z, a, l, i)
  
  # Collect output variables from environment
  out_vars <- ls()
  out      <- mget(out_vars)
  
  return(out)
  
}
