#' Initialize parameters
#' 
#' This function creates a list of matrices, which correspond to parameters of
#' neural network. The output is a list with parameters from `w1`, `b`, ..., to
#' `wl`, `bl` -- where `l` is the number of layers. The functions checks input
#' and output -- expected sizes of the matrices match the sizes of the layers:
#' 
#' `dim(wl) = c(l+1, l)`
#' `dim(bl) = c(l+1, 1)`
#'
#' @param ls list of layer sizes
#'
#' @return list of matrices
#' @export
#'
#' @examples
#' layers <- 1:6
#' set.seed(123)
#' layers <- as.list(sample(x = layers, size = 1e4, replace = T))
#' system.time(init_params(ls = layers))
init_params <- function(x, ls){
  
  m   <- dim(x)[2]  ## number of observations
  l   <- length(ls) ## number of layers in the network
  out <- list()     ## output list
  
  if (l < 2){stop("Neural network must have more than 1 layer")}
  if (!is.list(ls)){stop("Layer size must be list")} 
  
  for (i in 2:l){
    nl1 <- ls[[i]]
    nl  <- ls[[i - 1]]
    
    w <- matrix(runif(nl1 * nl), nrow = nl1, ncol = nl, byrow = T) * 0.01
    b <- matrix(rep(0, nl1), nrow = nl1)
    
    out[[paste0("w", i - 1)]] <- w
    out[[paste0("b", i - 1)]] <- b
    
    ch1 <- all(dim(out[[paste0("w", i - 1)]]) == c(nl1, nl))
    ch2 <- all(dim(out[[paste0("b", i - 1)]]) == c(nl1, 1))
    
    if (!ch1){stop("Check W matrix dimensions")}
    if (!ch2){stop("Check b matrix dimensions")}
  }
  
  return(out)
}
