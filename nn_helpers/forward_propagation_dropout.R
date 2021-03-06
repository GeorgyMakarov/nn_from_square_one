#' Forward propagation with dropout
#' 
#' Computes forward propagation by taking input matrix of features variables
#' and initial parameters. Randomly drops out some neurons, defined by `kp`
#' constant, which stands for keep probabilities.
#'
#' @param x feature matrix
#' @param params list of initial parameters
#' @param actif vector of activation functions
#' @param layers vector of layers
#' @param kp numeric scalar, keep probability
#'
#' @return list of z and a
#' @export
forward_propagation_dropout <- function(x, params, actif, layers, kp){
  
  l <- length(layers) - 1
  a <- x
  
  for (i in 1:l){
    f <- actif[i]
    w <- params[[paste0('w', i)]]
    b <- params[[paste0('b', i)]]
    z <- linear_fwd(a, w, b)
    a <- activation_fwd(z, f)
    
    # Shut down some neurons of each layer, but not the last layer. We must keep
    # all the neurons of the last layer, because we need them to compute the
    # error while training the model.
    v <- dim(a)[1] * dim(a)[2]
    d <- matrix(rnorm(v), nrow = dim(a)[1], ncol = dim(a)[2])
    d[d < kp] <- 0
    
    if (i < max(l)){
      a <- a * d  ## dimensions must be the same as original a
      a <- a / kp ## scale back to original scale
    }
    
    assign(x = paste0('w', i), value = w)
    assign(x = paste0('z', i), value = z)
    assign(x = paste0('b', i), value = b)
    assign(x = paste0('a', i), value = a)
    assign(x = paste0('d', i), value = d)
  }
  
  # Remove all variables that we do not need in the output. This is
  # required to be able to handle as many layers as required.
  rm(x, params, actif, layers, f, w, b, z, a, l, i, d, v, kp)
  
  # Collect output variables from environment
  out_vars <- ls()
  out      <- mget(out_vars)
  
  return(out)
  
}