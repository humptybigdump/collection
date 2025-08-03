import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Read data into a DataFrame
data = pd.read_excel('data_6_1.xlsx')
np.random.seed(2)
# Extract and transform data
C1_Data = np.array(data[['Class1_X1', 'Class1_X2']].dropna()).T / 100
C2_Data = np.array(data[['Class2_X1', 'Class2_X2']].dropna()).T / 100
C1 = np.vstack((np.ones(C1_Data.shape[1]), C1_Data, C1_Data[0, :] ** 2))
C2 = np.vstack((np.ones(C2_Data.shape[1]), C2_Data, C2_Data[0, :] ** 2))

# 3D Plot
fig = plt.figure(1)
ax = fig.add_subplot(111, projection='3d')
ax.scatter(C1[1, :], C1[2, :], C1[3, :], c='b', marker='s', s=144, linewidth=3, label='Class 1')
ax.scatter(C2[1, :], C2[2, :], C2[3, :], c='r', marker='s', s=144, linewidth=3, label='Class 2')
ax.set_xlabel('x_1')
ax.set_ylabel('x_2')
ax.set_zlabel('x_1^2')
ax.legend()

# Perceptron Algorithm
t_steps = 10000
eta = 0.01
X = np.vstack((C1.T, C2.T))
T = np.hstack((np.ones(C1.shape[1]), -1*np.ones(C2.shape[1])))
W = np.random.rand(X.shape[1])
W_list, err = [], []
for i in range(t_steps+1):
    index = np.random.randint(0, X.shape[0])
    wx = np.dot(W, X[index, :])
    y = 1 if wx > 0 else -1
    W = W - eta * (y - T[index]) * X[index, :]
    if i%50 == 0:
        W_list.append(W.copy())
 
       
        

# Hyperplane
A, B = np.meshgrid(np.arange(0, 3.1, 0.1), np.arange(0, 2.1, 0.1))

plt.figure(3)
plt.plot(C1[1, :], C1[2, :], 'b*')
plt.plot(C2[1, :], C2[2, :], 'r*')

for i,W in enumerate(W_list):
    Z = -(W[0] + W[1] * A + W[2] * B) / W[3]
    

    # Decision Boundary
    bound_x1 = np.linspace(0, 3, 100)
    bound_x2 = -(1/W[2]) * (W[3] * bound_x1**2 + W[1] * bound_x1 + W[0])
    plt.plot(bound_x1, bound_x2, '-g', linewidth=0.2+1.4*i/len(W_list))
ax.plot_surface(A, B, Z, cmap='bone')
plt.ylim(0, 1.5)
plt.show()
