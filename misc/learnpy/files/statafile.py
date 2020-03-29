# reading stat files 


import pandas as pd 

df = pd.read_stata("files/disarea.dta")

print(df.describe())
print(df.head())
print(df.iloc[0:, 2:10])

import matplotlib.pyplot as plt 



pd.DataFrame.boxplot(df[['disa1', 'disa2']])
plt.show()


pd.DataFrame.hist(df[['disa10']])
plt.xlabel('Extent of disease')
plt.ylabel('Number of countries')
plt.show()
