elu_backward <- function(alpha = 0.01, z){
  g <- z
  g[g >= 0] <- 1
  g[g < 0]  <- alpha * exp(z)
  return(g)
}