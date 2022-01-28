import numpy as np
import os

"""
Source helper files. Set working directory one level up before sourcing.
"""
os.path.dirname(os.path.abspath('l_layer_network.py'))
exec(open('./helpers_py/get_layer_py.py').read())
exec(open('./helpers_py/init_py.py').read())
exec(open('./helpers_py/linear_fwd.py').read())


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

l = [4, 2]

layers = get_layer_py(x, y, l)
print(layers)

l[0]



