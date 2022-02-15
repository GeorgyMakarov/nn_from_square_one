#' Initialize Standard
#' 
#' Initialize parameters with random small numbers.
#'
#' @param nl1 size of current level
#' @param nl size of size of previous level
#'
#' @return weight matrix
#' @export
init_stand <- function(nl1, nl){
  w <- matrix(runif(nl1 * nl), nrow = nl1, ncol = nl, byrow = T) * 0.01
  return(w)
}