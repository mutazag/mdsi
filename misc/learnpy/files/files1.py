

#open files 
filename = "files/titanic_sub.csv"
file = open (filename, mode='r') # r is for read only 
text = file.read()
file.close()

print(text)


# open with context 

with open (filename, mode='r') as file: 
    print(file.read())


