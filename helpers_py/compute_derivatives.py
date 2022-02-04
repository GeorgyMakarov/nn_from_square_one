import numpy as np
import os

a = np.array([0.5, 0.5, 0.5])
y = np.array([0, 1, 1])

a = np.array(a).reshape(1, 3)
y = np.array(y).reshape(1, 3)

dal1 = np.divide(y, a)
dal2 = np.divide(1 - y, 1 - a)

dal  = -(dal1 - dal2)2da
