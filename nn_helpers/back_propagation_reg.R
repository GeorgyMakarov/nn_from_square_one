#' Back propagation with regularization
#' 
#' Computes back propagation for neural net with regularization parameter
#' defined by `lambda` constant.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param cache list of forward propagation results
#' @param param list of initial parameters
#' @param l list of layers sizes
#' @param activation vector of activation functions names
#' @param lambda numeric scalar, regularization parameter
#'
#' @return list of derivatives
#' @export
back_propagation_reg <- function(x, y, param, cache, l, activation, lambda){
  
  m     <- dim(x)[2]  ## number of observations
  
  # Define last activation. Must be always l - 1. Required to compute dal
  last_a <- paste0('a', length(l) - 1)
  al     <- cache[[last_a]]
  
  # Check that activation is in cache
  if(!last_a %in% names(cache)){stop("Activation and levels do not match")}
  
  # Check that the last activation is either sigmoid or softmax -- otherwise
  # default to sigmoid.
  cond <- !any(c("sigmoid", "softmax") %in% activation[length(activation)])
  if (cond){stop("Last activation must be sigmoid or softmax")}
  
  
  # Compute initial dA for last activation. This becomes input for linear
  # activation.
  dal1 <- y / al
  dal2 <- (1 - y) / (1 - al)
  dal  <- -(dal1 - dal2)
  
  for (i in seq(length(l) - 1, 1)){
    
    # Define required variables
    if (i == 1){
      a_prev <- x
    } else {
      a_prev <- cache[[paste0('a', (i - 1))]]
    }
    
    w      <- cache[[paste0('w', i)]]
    z      <- cache[[paste0('z', i)]]
    actif  <- activation[[i]]
    li     <- l[[i + 1]]
    
    # Compute dz as by applying backwards activation functions
    dz <- activation_backward(da = dal, z = z, f = actif)
    dw <- (1 / m) * (dz %*% t(a_prev))
    dw <- dw + ((lambda / m) * w) ## add regularization
    db <- matrix((1 / m) * sum(dz), nrow = li)
    
    # Assign new variables
    assign(x = paste0('dz', i), value = dz)
    assign(x = paste0('dw', i), value = dw)
    assign(x = paste0('db', i), value = db)
    assign(x = paste0('da', i), value = dal)
    
    dal <- t(w) %*% dz
  }
  
  # Clean up local environment
  rm(x, y, param, cache, l, activation, m, last_a, al, dal1, dal2, dal,
     cond, i, a_prev, w, z, actif, li, dz, dw, db)
  
  # Get all output from environment
  out_vars <- ls()
  out      <- mget(out_vars)
  
  return(out)
  
}
