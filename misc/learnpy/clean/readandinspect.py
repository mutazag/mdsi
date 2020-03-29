#read and inspect a data set 

# Import pandas
import pandas as pd
# Read the file into a DataFrame: df
df = pd.read_csv('clean/dob_job_application_filings_subset.csv')


# Print the head of df
print(df.head())

# Print the tail of df
print(df.tail())

# Print the shape of df
print(df.shape)

# Print the columns of df
print(df.columns)

# print info
print(df.info())


# freq count 
print(df.Borough.value_counts(dropna = False))
print(df['Borough'].value_counts(dropna = False))
print(df['Zip'].value_counts(dropna = False).head())
print(df['Non-Profit'].value_counts(dropna =False)) # missing values


# summary stats on numeric columns 
df_describe = df.describe()

df.describe()['Job #']['count']
# df.describe()[[1],['Job #']]
df_describe.iloc[[0,1,2]]
df_describe.loc[['count','mean'], ['Job #', 'Block']]
