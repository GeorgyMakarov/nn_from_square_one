def linear_fwd(a, w, b):
  """
  a -- activations from previous layer: (size of prev layer, m)
  w -- weights matrix: (size of curr layer, size of prev layer)
  b -- bias matrix: (size of current layer, 1)
  """
  
  z     = np.dot(w, a) + b
  assert(z.shape == (w.shape[0], a.shape[1]))
  cache = (a, w, b)
  return z, cache
  
