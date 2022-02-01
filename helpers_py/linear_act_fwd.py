def linear_act_fwd (a_prev, w, b, act):
  
  """
  a_prev -- activations from previous layer:(size of previous layer, m)
  w      -- weights matrix: (size of current layer, size of previous layer)
  b      -- bias vector: (size of current layer, 1)
  act    -- activation function to be used in current layer
  """
  
  if act == "sigmoid":
    z, linear_cache     = linear_fwd(a_prev, w, b)
    a, activation_cache = sigmoid(z)
  
  elif act == "relu":
    z, linear_cache     = linear_fwd(a_prev, w, b)
    a, activation_cache = relu(z)
    
  assert(a.shape == (w.shape[0], a_prev.shape[1]))
  cache = (linear_cache, activation_cache)
  
  return a, cache
