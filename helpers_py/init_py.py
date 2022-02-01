def init_py(ld):
  """
  ld -- list containing dimensions of each layer
  """
  
  np.random.seed(3)
  parameters = {}       ## list to store output
  l          = len(ld)  ## number of layers in network
  
  for i in range(2, l + 1):
    parameters['w' + str(i - 1)] = np.random.randn(ld[i], ld[i - 1]) * 0.01
    parameters['b' + str(i - 1)] = np.zeros((ld[i], 1))
     
    assert(parameters['w' + str(i - 1)].shape == (ld[i], ld[i - 1]))
    assert(parameters['b' + str(i - 1)].shape == (ld[i], 1))
    
  return parameters
