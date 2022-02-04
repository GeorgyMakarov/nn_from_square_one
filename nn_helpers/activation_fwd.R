#' Compute activation forward
#' 
#' Applies activation function to results of linear forward.
#'
#' @param z result of linear forward
#' @param activation character string, activation function name
#'
#' @return list of activations
#' @export
activation_fwd <- function(z, activation){
  f_txt <- paste0(activation, "(z)")
  a     <- eval(parse(text = f_txt))
  return(a)
}
