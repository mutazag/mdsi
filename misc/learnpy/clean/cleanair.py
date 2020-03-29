# reshaping air quality 


import pandas as pd 
airquality = pd.read_csv("clean/airquality.csv")

print(airquality.head())
print(airquality.columns)
# columns Ozone, Solar.R, Wind, Temp are variables, not values, each row has mutiple obs

# melt

airquality_melt = pd.melt(
    frame = airquality,
    id_vars= ['Month', 'Day'], 
    value_vars = ['Ozone', 'Solar.R', 'Wind', 'Temp'], 
    var_name='measurement', 
    value_name='reading' )


print(airquality_melt.head())

df1 = airquality_melt[(airquality_melt.Day == 1) & (airquality_melt.Month == 5)]


print(df1.head())
df1_pivot = df1.pivot_table(
    index = ['Month', 'Day'], 
    values = 'reading', 
    columns = 'measurement')

print(df1_pivot.head())
print(df1_pivot.index)

df1_pivot = df1_pivot.reset_index()

print(df1_pivot.head())
print(df1_pivot.index)


### tidy u the tb data set 

tb = pd.read_csv("clean/tb.csv")
print(tb.head())
# melth
tb = pd.melt(
    frame = tb, 
    id_vars = ['country', 'year']
)

print(tb.head(10))

tb['sex'] = tb.variable.str[0]
tb['age_group'] = tb.variable.str[1:]
print(tb.head(10))


# tidy up the ebola data set 

ebola = pd.read_csv("clean/ebola.csv")
print(ebola.head())
print(ebola.info())
print(ebola.describe())

ebola_melt = pd.melt(
    frame=ebola,
    id_vars = ['Date', 'Day'], 
    var_name = 'type_country', 
    value_name = 'count' )

print(ebola_melt.head(10))

ebola_melt['str_split'] = ebola_melt.type_country.str.split('_')
print(ebola_melt.head(10))

# Create the 'type' column
ebola_melt['type'] = ebola_melt.str_split.str[0]
# Create the 'country' column
ebola_melt['country'] = ebola_melt.str_split.str.get(1)
print(ebola_melt.head(10))