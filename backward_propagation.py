import numpy as np

a2 = np.array([[4.799010e-05, 5.234009e-05], [5.196547e-05, 6.274118e-05]])
a3 = np.array([1.399542e-07, 1.551912e-07])
w3 = np.array([0.002460877, 0.00420593])
y  = np.array([0, 1])

dz3 = a3 - y
da2 = np.dot(w3.T, dz3)
dz2 = np.multiply(da2, np.int64(a2 > 0))
