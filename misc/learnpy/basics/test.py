#  in console run py -m pip install matplotlib 
import matplotlib.pyplot as plt
import numpy as np

# x = np.linspace(0, 20, 100)  # Create a list of evenly-spaced numbers over the range
# plt.plot(x, np.sin(x))       # Plot the sine of each x point
# plt.show()                   # Display the plot



print("S")

h  = [1.7, 1.8]
w = [70, 73]

print(h)

np_h  = np.array(h)
np_w = np.array(w)

bmi = np_w / np_h ** 2

print(bmi)
print(bmi[0])
print(bmi[1])

print(bmi[bmi > 23])

print(type(bmi))

print("end")


nn = np.array([0,1,2,3,4,5])
mm = nn[2:4]
print(mm)

print(type(nn))

np_all = np.array([h,w])


print(np_all)

print(np_all[0,0])
print(np_all[:,0])


print("ssssssss")
x = np.array([[1, 2, 3],
              [1, 2, 3]])
y = np.array([[1, 1, 1],
              [1, 2, 3]])

print(x)
print(y)
z = x - y

print(z)




print ("xxxx")
baseball = [[180, 78.4],
            [215, 102.7],
            [210, 98.5],
            [188, 75.2]]


np_baseball = np.array(baseball)

print(np_baseball.shape)