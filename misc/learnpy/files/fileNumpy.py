# importing files using nympy 
# import data as arrays

import numpy as np 


fn = "files\mnist_kaggle_some_rows.csv" 
data = np.loadtxt(fn, delimiter = ',')
print(data)
print(type(data))
print(np.shape(data))


titanic = np.recfromcsv("files/titanic_sub.csv") 
np.shape(titanic)
print(titanic[:4])
print(type(titanic[1]))
