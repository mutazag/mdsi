from urllib.request import urlretrieve

## download file using urlretrieve 
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
filename = "files/winequality-white.csv"
urlretrieve(url,filename)

# Read file into a DataFrame and print its head
import  pandas as pd    
df = pd.read_csv(filename, sep=';')
print(df.head())

## or import into pd df directly from url

df2 = pd.read_csv(url, sep=";")
print(df.head())


import matplotlib.pyplot as plt
pd.DataFrame.hist(df2.ix[:, 0:1])
plt.xlabel('fixed acidity (g(tartaric acid)/dm$^3$)')
plt.ylabel('count')
plt.show()

# import xls from web 

# Assign url of file: url
url = 'http://s3.amazonaws.com/assets.datacamp.com/course/importing_data_into_r/latitude.xls'


# Read in all sheets of Excel file: xl
xl = pd.read_excel(url, sheetname = None)
print(xl.keys())
print(xl['1700'])



# GET requests 
from urllib.request import urlopen, Request 

url = "http://www.datacamp.com/teach/documentation"
request = Request(url)
response = urlopen(request)
print(type(response))

html = response.read()
print(html)
response.close()

# using requests package 
import requests
url = "http://www.datacamp.com/teach/documentation"
r = requests.get(url)
text = r.text
print(text)
r.close()


# BeautifulSoup 

# Import packages
import requests
from bs4 import BeautifulSoup

# Specify url: url
url = 'https://www.python.org/~guido/'

# Package the request, send the request and catch the response: r
r = requests.get(url)

# Extracts the response as html: html_doc
html_doc = r.text

# Create a BeautifulSoup object from the HTML: soup
soup = BeautifulSoup(html_doc)

# Prettify the BeautifulSoup object: pretty_soup
pretty_soup = soup.prettify()

# Print the response
print(pretty_soup)

# Get the title of Guido's webpage: guido_title
guido_title = soup.title


# Print the title of Guido's webpage to the shell
print(guido_title)

# Get Guido's text: guido_text
guido_text = soup.get_text()

# Print Guido's text to the shell
print(guido_text)


# Find all 'a' tags (which define hyperlinks): a_tags
a_tags = soup.find_all("a")

# Print the URLs to the shell
for link in a_tags:
    print(link.get('href'))

