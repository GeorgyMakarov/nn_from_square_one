predict_nn <- function(x, y, md, f, conv = F, cut_off){
  hn       <- md[['hn']]
  l_size   <- get_layer_size(x, y, hn)
  params   <- md[['params']]
  fwd_prop <- forward_propagation(x, params, f, l_size)
  
  # Get last activation values depending on number of layers
  a        <- paste0('a', length(l_size) - 1)
  out      <- fwd_prop[[a]]
  
  # Convert probabilities to binary values based on cut_off
  # Use vectorized function to increase computation speed
  if (conv){
    out[out >= cut_off] <- 1
    out[out <  cut_off] <- 0
  }
  return(out)
}