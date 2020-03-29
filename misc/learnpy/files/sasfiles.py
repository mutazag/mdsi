# importing froms from SAS format 

import pandas as pd
from sas7bdat import SAS7BDAT

with SAS7BDAT("files/sales.sas7bdat") as file: 
    df = file.to_data_frame() 

print(df.describe())
print(type(df))


import matplotlib.pyplot as plt


df.boxplot(['S','P'])
# plt.ylabel('S')
plt.show()

df.hist('P')
plt.ylabel("P value")
plt.show()


df.plot('YEAR', ['P', 'S'], kind='line')
plt.show()

df.plot.scatter(x='P', y='S')
plt.show()

