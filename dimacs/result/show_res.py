import numpy as np

dat = np.matrix([list(map(float, input().split())) for _ in range(10)])
print(np.mean(dat, axis=0))
print(np.std(dat, axis=0))