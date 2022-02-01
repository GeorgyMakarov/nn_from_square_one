def sigmoid(z):
  
  a     = 1 / (1 + np.exp(-z))
  cache = z
  return a, cache
