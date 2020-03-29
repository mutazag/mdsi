#using matplotlib

import matplotlib.pyplot as plt
import numpy as np


print("basic plot")
a = [1, 2, 3, 4]
b = [3, 9, 2, 6]
plt.plot(a, b)
plt.show()
# plt.clf()

plt.scatter(a,b)
plt.show()
# plt.clf()


print("histogram")

height = np.round(np.random.normal(1.75, .20, 5000), 2)
weight = np.round(np.random.normal(60.32, 15, 5000), 2)

plt.hist(height, bins=50)
plt.xlabel("height")
plt.title("Height Distribution")
plt.yticks([100,200,300],["One Hundrades", "Two", "Three"])
plt.show()