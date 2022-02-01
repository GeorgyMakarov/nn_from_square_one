import numpy as np
import os

"""
Source helper files. Set working directory one level up before sourcing.
"""
os.path.dirname(os.path.abspath('l_layer_network.py'))
exec(open('./helpers_py/get_layer_py.py').read())
exec(open('./helpers_py/init_py.py').read())
exec(open('./helpers_py/linear_fwd.py').read())
exec(open('./helpers_py/linear_act_fwd.py').read())
exec(open('./helpers_py/sigmoid.py').read())
exec(open('./helpers_py/relu.py').read())


"""
Create input matrix of feature variables
"""
x  = np.array([[0.15, 0.11, 0.12], [0.20, 0.28, 0.30]])
xa = 2
xb = 3
x  = np.array(x).reshape(xa, xb)
print(x)

"""
Create input matrix of output variable
"""
y  = np.array([0, 1, 1])
ya = 1
yb = 3
y  = np.array(y).reshape(ya, yb)
print(y)

l = [5, 2]

layers = get_layer_py(x, y, l)
print(layers)

parameters = init_py(ld = layers)
print(parameters)

z, linear_cache = linear_fwd(x, parameters['w1'], parameters['b1'])
print("z = " + str(z))

a, lac = linear_act_fwd(x, parameters['w1'], parameters['b1'], "sigmoid")
a, lac = linear_act_fwd(x, parameters['w1'], parameters['b1'], "relu")


"""
Manual data testing
"""
w1 = np.array([[0.1, 0.2], [0.3, 0.4], [0.5, 0.6], [0.7, 0.8], [0.9, 1.0]])
w1 = np.array(w1).reshape(5, 2)
b1 = np.array([[0.1], [0.2], [0.3], [0.4], [0.5]])
b1 = np.array(b1).reshape(5, 1)

z, lin = linear_fwd(x, w1, b1)
a, aca = linear_act_fwd(x, w1, b1, "sigmoid")


