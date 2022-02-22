init_adam <- function(p){
  nms <- lapply(X   = names(p),
                FUN = function(id){paste0('d', id)})
  nms <- as.character(nms)
  v <- lapply(X   = names(p), 
              FUN = function(id){
                matrix(data = rep(0, dim(p[[id]])[1] * dim(p[[id]])[2]),
                       nrow = dim(p[[id]])[1],
                       ncol = dim(p[[id]])[2])
              })
  names(v) <- nms
  s <- v
  res <- list("v" = v, "s" = s)
  return(res)
}