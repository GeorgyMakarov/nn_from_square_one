tanh_backward <- function(x){
  tx <- tanh(x)
  tx <- tx ^ 2
  tx <- 1 - tx
  return(tx)
}