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
  browser()
  
  # We need sum of squares for regularization
  # TODO: add eval parse text as sum of all weights -- release through function
  # TODO: write justification for eval vs other options -- large matrix
  system.time(sum(w1 ^ 2) + sum(w2 ^ 2) + sum(w3 ^ 2))
  
  txt <- "sum(w1 ^ 2) + sum(w2 ^ 2) + sum(w3 ^ 2)"
  tmp <- eval(parse(text = txt))
  
  
  # Compute cross entropy cost
  ce_cost <- compute_cost(x, y, cache, l)
  
  # Compute regularization
  
  
  # Compute output cost with regularization
  cost <- ce_cost + reg_cost
  return(cost)
}