def init_py(ld):
  """
  ld -- list containing dimensions of each layer
  """
  
  np.random.seed(123)
  parameters = {}               ## list to store output
  l          = len(layer_dims)  ## number of layers in network
  
  for l in range(1, l):
    parameters['w' + str(l)] = np.random.randn(layer_dims[l], layer_dims[l - 1]) * 0.01
    parameters['b' + str(l)] = np.zeros((layer_dims[l], 1))
  
  return parameters
