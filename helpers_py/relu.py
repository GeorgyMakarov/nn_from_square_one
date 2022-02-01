def relu(z):
  a     = np.maximum(0, z)
  cache = z
  assert(z.shape == a.shape)
  return a, cache
