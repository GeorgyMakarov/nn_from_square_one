fwd_propagation <- function(x, par, arch, ls, alpha){
  
  m     <- dim(x)[2]            ## number of observations
  cache <- list()               ## prepare list for output
  l_hn  <- (length(par) / 2)    ## number layers = length of arch
  cond  <- l_hn != length(arch) ## check condition
  msgs  <- "Architecture is not equal to number of input and hidden layers"
  
  if (cond){stop(msgs)}         
  
  # Get initial parameters from the list of parameters
  for (i in 1:length(par)){assign(names(par)[i], par[[names(par)[i]]])}
  rm(i)
  
  # Assign adjusted b matrices. Required to add `b` to mat mult result of w and
  # x to get z.
  all_b <- grep(pattern = "b", x = names(par))
  for (i in 1:l_hn){
    b <- par[[all_b[i]]]
    assign(paste0("b", i, "_adj"), matrix(rep(b, m), nrow = dim(b)[1]))
    rm(i, b)
  }
  
  # Compute activation functions. Programmatically control the number of
  # activation to be computed. Consider different computation for 1 layer and
  # other layers.
  for (i in 1:l_hn){
    if (i == 1){
      
      w    <- get(x = "w1")
      b    <- get(x = "b1_adj")
      f_nm <- paste0(arch[i], '(cache[["z1"]])')
      cache[["z1"]] <- w %*% x + b
      cache[["a1"]] <- eval(parse(text = f_nm))
      # cache[["b1"]] <- b
      
    } else {
      
      w_name <- paste0("w", i)
      b_name <- paste0("b", i, "_adj")
      a_name <- paste0("a", i - 1)
      
      z_out <- paste0("z", i)
      a_out <- paste0("a", i)
      b_out <- paste0("b", i)
      
      w <- get(x = w_name)
      b <- get(x = b_name)
      a <- cache[[a_name]]
      
      f_nm <- paste0('cache[["', z_out, '"]]')    
      
      # If function has parameter alpha -- add it to the text of function to
      # evaluate together with other parameters.
      if (arch[i] %in% c("elu", "p_relu")){
        f_nm <- paste0(arch[i], '(alpha = alpha, x = ', f_nm, ')')
      } else {
        f_nm <- paste0(arch[i], '(x = ', f_nm, ')') 
      }
      
      cache[[z_out]] <- w %*% a + b
      cache[[a_out]] <- eval(parse(text = f_nm))
      # cache[[b_out]] <- b
      
    }
  }
  
  return(cache)
}
