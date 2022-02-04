#' Linear module forward
#' 
#' Computes linear output when doing forward propagation in neural network.
#' The output computation formula is `z = w %*% a + b`.
#'
#' @param a activation matrix
#' @param w weight matrix
#' @param b bias matrix
#'
#' @return list
#' @export
linear_fwd <- function(a, w, b){
  z   <- w %*% a + as.vector(b)
  
  ch1 <- dim(z)[1] == dim(w)[1]
  ch2 <- dim(z)[2] == dim(a)[2]
  
  if (!ch1)(stop("Check dimensions of weight matrix"))
  if (!ch2)(stop("Check dimensions of activation matrix"))
  
  return(z)
}
