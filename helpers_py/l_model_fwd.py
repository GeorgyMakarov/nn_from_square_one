def l_model_fwd(x, params):
  caches = []
  a      = x
  l      = len(params) // 2  ## number of layers in neural network
  
  for i in range(1, l):
    a_prev   = a
    a, cache = linear_act_fwd(a_prev, parameters['w' + str(i)], parameters['b' + str(i)], "relu")
    caches.append(cache)
    print(caches)
  
  al, cache  = linear_act_fwd(a_prev, parameters['w' + str(l)], parameters['b' + str(l)], "sigmoid")
  caches.append(cache)
  
  assert(al.shape == (1, x.shape[1]))
  
  return al, caches
    
