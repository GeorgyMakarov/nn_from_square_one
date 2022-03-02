import numpy as np
import os

os.path.dirname(os.path.abspath('l_layer_network.py'))
exec(open('./helpers_py/temp_model.py').read())
model(n = 10, m = 100, epochs = 10)

