x = ["p", "y", "t", "h", "o", "n"]
print(x[0:5])


import pandas as pd
marketing = pd.read_csv("https://goo.gl/6A6Qe2")


print(marketing.loc[["a", "b"], ["Views", "Clicks"]])

import numpy as np


x = np.array([1, 2, False, True, 3])
print(x)

import numpy as np
x = np.array([1, 2, False, True, "3"])
print(x)

print(type(x))


import numpy as np
x = np.array([7, 7, 5, 4, 5, 5, 5, 7])
y = np.array([4, 2, 9, 0, 5, 1, 6, 8])
print(np.correlate(x, y))
print(np.corrcoef(x, y))


x = [9, 12, 4, 7]
x.reverse()
print(x)


p = ["D", 2, "E", 5, "F", 4]
q = p.append(['X', 8])

print(q)



import numpy as np
x = np.array([11, 12, 17, 15, 18])
x_small = x[x < 16]
print(x_small)


# Find the mean of the first column of costs
import numpy as np
costs = np.column_stack(([2, 2, 3, 1, 3, 3, 3, 2], 
                         [4, 4, 4, 7, 7, 7, 4, 7]))
# mean_costs = mean(costs[0:1,0])
print(np.mean(costs[0]))


import numpy as np
np_2d = np.array([[4, 7, 8], 
                  [19, 5, 18]])
print(np_2d[0][1])


x = "cautioned"
print(x.replace("u", "+"))

import numpy as np
x = np.array([3, 2, 9, 5, 7])
bool_x = x >= 7
print(bool_x)



import pandas as pd
fruits = pd.read_csv("https://goo.gl/DOw6pe")
print(fruits.loc[[0],["Bananas"]])


a = 7
b = [0, 1]
c = [3, 9, "True"]
print([a,b,c])



x = [11, 12, 13, 14]
y = x
y[2:4] = [15, 16]
print(x)


import pandas as pd
classes = pd.read_csv("https://goo.gl/JvBiH4")
classes["Course"] = ["Math", "Math", "Science"] 
print(classes)


x = 4.123412
print(int(x))



import pandas as pd
stores = pd.read_csv("https://goo.gl/LN5wGF")
print(stores.loc["a"])


x = True
y = "x is:"
print(y + str(x))


import numpy as np
np_2d = np.array([[1, 2, 3], 
                  [17, 18, 19]])
print(np_2d[1:, 0:])



# Print the number of occurrences of the string "b" in list x
x = ["b", "c", "c", "a", "b", "a"]
print(x
.count("b")
)



import numpy as np
x = np.array([[2, 6, 4], [1, 2, 2]])
y = np.array([[6, 2, 4], [2, 2, 1]])
print(x - y)


x = [9, "H", "M", 3, "R", 11]
del(x[1:3])
print(x)


import matplotlib.pyplot as plt

plt.show()


print(True or not(True))

# Find the standard deviation of x
import numpy as np
x = np.array([0.4, 1.2, 1.1])
print(np.std(x))



x = 5
y = -2
z = -1
print(
[y,z,x]
)



x = ["A", "B", "C", "D", "E", "F"]
print(x[2] + x[5])



import numpy as np
store = [3, 4, 5, 3, 4, 5]
cost = [94, 87, 81, 96, 97, 92]
np_cols = np.column_stack((store, cost))
print(np_cols)


x = 2
if not x:
    print("First attempt")
elif x % 2 == 0:
    print("Second attempt")
else:
    print("Final attempt")

x = [2, 5, 4, 0, 7, 1]
print(x[0])





import numpy as np
store = np.array(["A", "A", "B", "B", "B", "C", "C"])
cost = np.array([27, 22, 26, 30, 24, 25, 21])
select_cost = cost[store == "A"]
print(select_cost)


x = [10, 7, 8, 4, 9, 6]
print(x[-2]+x[-6])



x = [12, 2, 5, 15, 4, 1]
print(x[-5:])


x = [5, 8, 2, 3, 4, 1]
print(min(x))


# Print the number of occurences of the letter "e" in x
x = "this sentence has no meaning"
print(x
.count("e")
)



foo = [True, 3.2, "Apples", 0, "1.2"]
foo[1:3] = [8,1]
print(foo)



x = [6, 15, 19, 8, 18, 1]
print(sorted(x, reverse = False))



import numpy as np
x = np.array([2, 6, 4])
y = np.array([2, 1, 1])
print(x / y)