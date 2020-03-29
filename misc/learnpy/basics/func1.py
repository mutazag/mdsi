def f1(v1): 
    ''' function doc string '''
    v = v1 ** 2
    return v


d = f1(4)
print(d)

#---- 

## functions returning tuples 

def pow(v1, v2): 
    p1 = v1 ** v2
    p2 = v2 ** v2
    t = (p1, p2)
    return t

rett = pow(2,4)
print(rett)
a, b = rett 
print(a)
print(b)


## lambda functions and map

# Create a list of strings: spells
spells = ["protego", "accio", "expecto patronum", "legilimens"]
# Use map() to apply a lambda function over spells: shout_spells
shout_spells = map(lambda x: x + "!!!", spells)
# Convert shout_spells to a list: shout_spells_list
shout_spells_list = list(shout_spells)
# Convert shout_spells into a list and print it
print(shout_spells_list)



### using the filter function with lambda 

# Create a list of strings: fellowship
fellowship = ['frodo', 'samwise', 'merry', 'pippin', 'aragorn', 'boromir', 'legolas', 'gimli', 'gandalf']

# Use filter() to apply a lambda function over fellowship: result
result = filter(lambda x : len(x) > 6, fellowship)

# Convert result to a list: result_list
result_list = list(result)

# Convert result into a list and print it
print(result_list)


### using the reduce function 
# Import reduce from functools
from functools import reduce

# Create a list of strings: stark
stark = ['robb', 'sansa', 'arya', 'brandon', 'rickon']

# Use reduce() to apply a lambda function over stark: result
result = reduce(lambda x,y : y + x, stark)

# Print the result
print(result)