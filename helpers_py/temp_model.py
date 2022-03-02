def model(n, m, epochs):
  seed = 123
  for i in range(epochs):
    cost_total = 0
    seed       = seed + 1
    
    for k in range(n):
      cost        = np.random.randn()
      cost_total += cost
      
    cost_avg = cost_total / m
    
    print("Cost average is " + str(cost_avg))
