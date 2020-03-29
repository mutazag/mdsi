# pandas : working with files 


import pandas as pd 

fn = "files/titanic_sub.csv"
df = pd.read_csv(fn)

print(df.head())

nparray = df.values
np.shape(nparray)
print(type(nparray[1][1:4]))
print(nparray[1][1:4])

import matplotlib.pyplot as plt

pd.DataFrame.hist(df[['Age']])
plt.xlabel('Age (years)')
plt.ylabel('count')
plt.show()


## files in working directory 

import os
wd = os.getcwd()
print (wd)
os.listdir(wd)



## save df as pickle 
import pickle
pickle.dump(df, open("files/titanic.pkl", mode='wb'))


## load a pickle file 
df2 = pickle.load(open("files/titanic.pkl", "rb"))
print(type(df2))
print(df2.head())