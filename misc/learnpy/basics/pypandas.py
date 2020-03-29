# working with pandas

import pandas as pd 

df = pd.read_csv("dataset.csv",index_col=0)

print(df)

print(df.fname)
print(df["lname"])


# add columns
df["weight"] = [90,20]

print(df)

# mutate column 
df["age"] = df["age"] + 4
print(df)

# row access
print(df.loc[1])
print(df.loc[1]["age"])
