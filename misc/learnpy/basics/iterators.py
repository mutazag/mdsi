import sys

# practicing iterators and iterables 


ff = [ 'one', 'two', 'three', 'four']

print(ff)
# ff is iterable
# it is an iterator 
it = iter(ff)

print(next(it))
print(next(it))

print(it.__sizeof__())
print(*it)
try:    
    print(next(it))
except: 
    # sys.exc_info()
    print('end of iterator '); 




### iterators over range() and list
# Create a range object: values
values = range(10,21)
# Print the range object
print(values)
# Create a list of integers: values_list
values_list = list(values)
# Print values_list
print(values_list)
# Get the sum of values: values_sum
values_sum = sum(values)
# Print values_sum
print(values_sum)



### enumerators 
# Create a list of strings: mutants
mutants = ['charles xavier', 
            'bobby drake', 
            'kurt wagner', 
            'max eisenhardt', 
            'kitty pryde']

# Create a list of tuples: mutant_list
mutant_list = list(enumerate(mutants))

# Print the list of tuples
print(mutant_list)

# Unpack and print the tuple pairs
for index1, value1  in enumerate(mutants):
    print(index1, value1)

# Change the start index
for index2, value2 in enumerate(mutants, start = 1):
    print(index2, value2)


# now using zip 
aliases = ['prof x', 'iceman', 'nightcrawler', 'magneto', 'shadowcat']
powers = ['telepathy',
 'thermokinesis',
 'teleportation',
 'magnetokinesis',
 'intangibility']
mutant_data = list(zip(mutants, aliases))
# Create a zip object using the three lists: mutant_zip
mutant_zip = zip(mutants, aliases, powers)

# Print the zip object
print(mutant_zip)

# Unpack the zip object and print the tuple values
for value1, value2, value3 in mutant_zip:
    print(value1, value2, value3)


# unpack a zip with * operator 
mutant_zip = zip(mutants, aliases, powers)
print(*mutant_zip)


# unzip 
mutant_zip = zip(mutants, aliases, powers)
r1, r2, r3 = zip(*mutant_zip) # unzip using * and zip() 
print(r1)
print(r2)
print(r3) 


#### list comprehension 


doctor = ['house', 'cuddy', 'chase', 'thirteen', 'wilson']

# list of the firt char in each item in list doctors 
[doc[0] for doc in doctor]


 #list comprehension: conditionals 
[ num ** 2 for num in range(10) if num % 2 == 0]
[ (num, num ** 2) for num in range(10) if num % 2 == 0]
[ [num, num ** 2] for num in range(10) if num % 2 == 0]

#dictionary comprehensio 
{ num : -num for num in range(9)}

# exer

fellowship = ['frodo', 'samwise', 'merry', 'aragorn', 'legolas', 'boromir', 'gimli']
 [(member, len(member)) for member in fellowship]
[member for member in fellowship if len(member) >= 7]
# conditional in output expression 
[member if len(member) >= 7 else "" for member in fellowship]
# create a dict using comprehensions
{ member:len(member) for member in fellowship}




#### generators and generator functions

# List comprehension
fellow1 = [member for member in fellowship if len(member) >= 7]

# Generator expression
fellow2 = (member for member in fellowship if len(member) >= 7)

print(type(fellow1))
print(type(fellow2))

# generator object is treated as an iterator 
print(fellow2)
print(*fellow2)

#generator function using yeild 
def get_lengths(input_list):
    """Generator function that yields the
    length of the strings in input_list."""

    # Yield the length of a string
    for person in input_list:
        yield (person,len(person)) #yeild a tuple

for value in get_lengths(fellowship):
    print(value)