#' Compute cost with regularization
#' 
#' Computes the cost of forward propagation results with regularization.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param cache list of forward propagation results
#' @param l list of neural network layers
#' @param lambda regularization parameter
#'
#' @return numeric scalar
#' @export
compute_cost_reg <- function(x, y, cache, l, lambda){
  
  # Define parameters that need regularization
  # Prepare text that we will evaluate to compute the regularization
  m   <- dim(y)[2]
  nms <- names(cache)[grep(pattern = "w", x = names(cache))]
  lst <- c()
  for (i in nms){
    assign(x = i, value = cache[[i]])
    tmp <- paste0("sum(", i, " ^ 2)")
    lst <- c(lst, tmp)
  }
  
  # Prepare text of sum formula for evaluation. This way the sum is dynamic
  # depending on the number of layers
  txt <- paste(lst, collapse = " + ")
  
  # Compute cross entropy cost
  ce_cost <- compute_cost(x, y, cache, l)
  
  # Compute regularization
  k        <- lambda / (2 * m)
  reg_cost <- eval(parse(text = txt))
  reg_cost <- k * reg_cost
  
  # Compute output cost with regularization
  cost <- ce_cost + reg_cost
  
  return(cost)
}