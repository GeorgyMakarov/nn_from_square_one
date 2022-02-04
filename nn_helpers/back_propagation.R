#' Back propagation
#' 
#' Computes back propagation for neural net.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param cache list of forward propagation results
#' @param param list of initial parameters
#' @param l list of layers sizes
#' @param activation vector of activation functions names
#'
#' @return list of derivatives
#' @export
back_propagation <- function(x, y, param, cache, l, activation){
  
  grads <- list()     ## gradient storage
  m     <- dim(x)[2]  ## number of observations
  
  # Define last activation. Must be always l - 1. Required to compute dal
  browser()
  last_a <- paste0('a', length(l) - 1)
  al     <- cache[[last_a]]
  
  # Check that activation is in cache
  if(!last_a %in% names(cache)){stop("Activation and levels do not match")}
  
  # Check that the last activation is either sigmoid or softmax -- otherwise
  # default to sigmoid.
  if (!c("sigmoid", "softmax") %in% activation[length(activation)]){
    stop("Last activation must be sigmoid or softmax")
  }
  
  
  
  
  
  n_x <- l_size[['n_x']]
  n_h <- l_size[['n_h']]
  n_y <- l_size[['n_y']]
  a2  <- cache[['a2']]
  a1  <- cache[['a1']]
  w2  <- param[['w2']]
  
  dz2     <- a2 - y
  dw2     <- (1/m) * (dz2 %*% t(a1))
  db2     <- matrix((1/m) * sum(dz2), nrow = n_y)
  db2_adj <- matrix(rep(db2, m), nrow = n_y)
  
  dz1     <- (t(w2) %*% dz2) * (1 - a1 ^ 2)
  dw1     <- (1 / m) * (dz1 %*% t(x))
  db1     <- matrix((1 / m) * sum(dz1), nrow = n_h)
  db1_adj <- matrix(rep(db1, m), nrow = n_h)
  
  out <- list("dw1" = dw1, "db1" = db1, "dw2" = dw2, "db2" = db2)
  return(out)
}
