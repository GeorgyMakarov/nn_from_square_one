def get_layer_py(x, y, hn):
  """
  x  -- matrix of input layer:  (nrow = variables, ncol = m)
  y  -- matrix of output layer: (nrow = variables, ncol = m)
  hn -- list of hidden layers: defined by user
  """
  
  out   = {}           ## output list
  l_out = len(hn) + 2  ## total number of layers
  
  out[1]     = x.shape[0]
  
  for i in range(0, len(hn)):
    out[i + 2] = hn[i]
  
  out[l_out] = y.shape[0]
  
  return out
  
