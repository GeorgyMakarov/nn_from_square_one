forward_propagation <- function(x, params, actif, layers){
  
  l <- length(layers) - 1
  a <- x
  
  for (i in 1:l){
    f <- actif[i]
    w <- params[[paste0('w', i)]]
    b <- params[[paste0('b', i)]]
    z <- linear_fwd(a, w, b)
    a <- activation_fwd(z, f)
    assign(x = paste0('z', i), value = z)
    assign(x = paste0('a', i), value = a)
  }
  
  rm(x, params, actif, layers, f, w, b, z, a, l, i)
  
  browser()
  
}
