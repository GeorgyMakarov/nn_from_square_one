#' Initialize He
#' 
#' Initialize parameters with He method.
#'
#' @param nl1 size of current level
#' @param nl size of size of previous level
#'
#' @return weight matrix
#' @export
init_he <- function(nl1, nl){
  mult <- sqrt(2 / nl)
  w    <- matrix(runif(nl1 * nl), nrow = nl1, ncol = nl, byrow = T) * mult
  return(w)
}