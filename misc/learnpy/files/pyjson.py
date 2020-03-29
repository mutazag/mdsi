# loading and working with json files 


import json

with open('files/movies.json', 'r') as json_file: 
    jdata = json.load(json_file)

# print(jdata)

for k in jdata.keys(): 
    print( k + ": " , jdata[k])

print(jdata.keys())
print('Title: ', jdata['Title'])
print('Year: ', jdata['Year'])


# another one 
with open('files/config.json', 'r') as config_json_file: 
    config_json = json.load(config_json_file)
print(config_json)

config_json[0]
config_json[1]

 
for s in config_json: 
    print(s)
    print(s['tags'])
    for tag in s['tags']: 
        print("server: " + s['server'] + " - tag: " + tag)

# using api 

import requests
url = 'http://www.omdbapi.com/?apikey=72bc447a&t=the+social+network'
r = requests.get(url)

print(r.text)
jdata = r.json()
for k in jdata.keys(): 
    print(k + ": ", jdata[k])
print('end')