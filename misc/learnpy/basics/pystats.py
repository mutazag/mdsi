import numpy as np 

height = np.round(np.random.normal(1.75, .20, 5000), 2)
weight = np.round(np.random.normal(60.32, 15, 5000), 2)

np_city = np.column_stack((height, weight))


print("print mean for height and weight")
print("height mean: ",  np.round( np.mean(np_city[:,0]),2))

print("weight mean: ", np.round( np.mean(np_city[:,1]), 2))
