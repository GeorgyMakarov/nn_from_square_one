#' Get layer sizes
#' 
#' This function computes layer sizes of neural network. We need these sizes to
#' be able to create the architecture of the neural network.
#' 
#' The function computes the size of hidden layer as the nearest integer derived
#' from the mean of the neighbor layers. There is an alternative to provide the 
#' size of the hidden layers manually.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variables
#' @param hn number of hidden layer nodes per layer
#'
#' @return list
#' @export
#' 
#' @examples
#' x <- matrix(rnorm(n = 2 * 500), nrow = 2, ncol = 500, byrow = T)
#' y <- matrix(rnorm(n = 1 * 500), nrow = 1, ncol = 500, byrow = T)
#' unlist(get_layer_size(x, y))
#' unlist(get_layer_size(x, y, hn = 4))
#' unlist(get_layer_size(x, y, hn = c(4, 5, 3)))
#' unlist(get_layer_size(x, y, hn = c(4, NULL)))
get_layer_size <- function(x, y, hn = NULL){
  
  out <- list()
  
  if (is.null(hn)){
    out[[1]] <- dim(x)[1]
    out[[2]] <- round(mean(x = c(dim(x)[1], dim(y)[1])), 0)
    out[[3]] <- dim(y)[1]
  } else {
    out[[1]] <- dim(x)[1]
    for (i in 1:length(hn)){
      out[[i + 1]] <- hn[i]
    }
    l <- length(out)
    out[[l + 1]] <- dim(y)[1]
  }
  return(out)
}
