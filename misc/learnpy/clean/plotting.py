# Import pandas
import pandas as pd
import matplotlib.pyplot as plt
# Read the file into a DataFrame: df
df = pd.read_csv('clean/dob_job_application_filings_subset.csv')



df['Proposed Height'].plot('hist')
plt.show()


df.boxplot(column = ['Existing Height', 'Proposed Height'], by = 'Borough')
plt.show()



# Plot the histogram
df['Existing Zoning Sqft'].plot(kind='hist', rot=70, logx=True, logy=True)
plt.show()


# Create the boxplot
df.boxplot(column='Existing Zoning Sqft', by='Borough', rot=90)
plt.show()


#scatter plot
df.plot(kind='scatter', x='Proposed Height', y='Existing Height', rot=70)
plt.show()


