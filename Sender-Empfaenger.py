import numpy as np
import time
from scipy.linalg import null_space
from numpy import linalg as LA
import matplotlib as mpl
import matplotlib.pyplot as plt

from scipy.sparse import csc_matrix
from scipy.sparse.linalg import gmres

def calculate_Phi(x):
    value = 1/(x*x+1)
    return value

def calculate_D_Phi(x):
    value = -2*x/pow((x*x+1), 2)
    return value

def calculate_DD_Phi(x):
    value = (6*x*x-2)/pow((x*x+1), 3)
    return value

def calculate_f(z, y):
    sum = 0
    for k in range(0, len(z)):
        for n in range(0, len(y)):
            sum += calculate_Phi(LA.norm(z[k]-y[n]))
    value = sum
    return value

def calculate_F(z, y):
    sum = 0
    F_value = np.zeros((len(z), 2))
    for k in range(0, len(z)):
        for n in range(0, len(y)):
            sum += calculate_D_Phi(LA.norm(z[k]-y[n])) * \
                (1/LA.norm(z[k]-y[n]))*(z[k]-y[n])
        F_value[k] = sum
        sum = 0
    F_value = np.reshape(F_value, 2*len(z))
    return F_value

def calculate_Jacobi(z, y):
    part1 = np.zeros((1, 2))
    part2 = 0
    Df = np.zeros((2*len(z), 2*len(z)))
    for k in range(0, len(z)):
        for n in range(0, len(y)):
            distance = LA.norm(z[k]-y[n])
            D_Phi = calculate_D_Phi(distance)
            DD_Phi = calculate_DD_Phi(distance)
            part1 += (DD_Phi-(D_Phi/distance)) * \
                (1./pow(distance, 2))*(z[k]-y[n])*(z[k]-y[n]) + D_Phi/distance
            part2 += (DD_Phi-(D_Phi/distance)) * \
                (1./pow(distance, 2))*(z[k, 0]-y[n, 0])*(z[k, 1]-y[n, 1])
        Df[2*k, 2*k] = part1[0, 0]
        Df[2*k, 2*k+1] = part2
        Df[2*k+1, 2*k] = part2
        Df[2*k+1, 2*k+1] = part1[0, 1]
        part1 = np.zeros((1, 2))
        part2 = 0
    return Df

def Newton(x, y, tol):
    lst=[]
    k = 0
    F = calculate_F(x, y)
    k_max = 20
    alpha_min = 0.01
    while (LA.norm(F) > tol) & (k<=k_max):
        k += 1
        DF = calculate_Jacobi(x, y)
        dk = np.linalg.solve(DF, -F)
        #x0 = np.reshape(x, 2*len(x))
        #dk,exitCode = gmres(DF, -F, x0, tol)
        dk = np.reshape(dk, (len(x), 2))
        alpha = 1
        while (LA.norm(calculate_F(x+alpha*dk, y)) >= LA.norm(calculate_F(x, y))) & (alpha>alpha_min):
        #while (calculate_f(x+alpha*dk, y) <= calculate_f(x, y)) & (alpha>alpha_min):
            alpha = 0.5 * alpha
        x = x + alpha * dk
        F = calculate_F(x, y)
        print("k =",k,"\t alpha",alpha,"\t f(x_k)=",calculate_f(x, y),"\t |Df(x_k|",LA.norm(F))
        lst.append(x)
    return lst

# Empfängerpositionen 
y = 100*np.array([[0.76, 0.96],
             [0.2, 0.374],
             [0.187, 0.782],
             [0.87, 0.23]])
   
plt.figure()
plt.axis([0, 100, 0, 150])
#Sender
plt.plot(y[:, 0:1], y[:, 1:2], 'bH')
plt.pause(0.4)

input()

# Startwert Sender
z0 = 100*np.array([[0.7, 0.95],
             [0.26, 0.373],
             [0.2, 0.681]])

zfinal = Newton(z0, y, 1e-7)

print(zfinal[-1])

iterations = len(zfinal)
Weg1 = np.zeros((iterations+1,2))
Weg2 = np.zeros((iterations+1,2))
Weg3 = np.zeros((iterations+1,2))
Weg4 = np.zeros((iterations+1,2))
Weg1[0,:]= z0[0,:]
Weg2[0,:]= z0[1,:]
Weg3[0,:]= z0[2,:]
for i in range (1,iterations+1):
    Weg1[i,:] = zfinal[i-1][0,:]
    Weg2[i,:] = zfinal[i-1][1,:]
    Weg3[i,:] = zfinal[i-1][2,:]

#Startposition
plt.axis([0, 100, 0, 150])
plt.plot(y[:, 0:1], y[:, 1:2], 'bH')
plt.plot(z0[:, 0:1], z0[:, 1:2], 'gx')
plt.plot(Weg1[0,0], Weg1[0,1], 'yx--', Weg2[0,0], Weg2[0,1], 'gx--',
         Weg3[0,0], Weg3[0,1],'cx--')
plt.pause(0.4)

input()

for i in range (1,iterations+1):
    plt.axis([0, 100, 0, 150])
    plt.plot(y[:, 0:1], y[:, 1:2], 'bH')
    plt.plot(z0[:, 0:1], z0[:, 1:2], 'g*')
    plt.plot(Weg1[0:i,0], Weg1[0:i,1], 'yx--', Weg2[0:i,0], Weg2[0:i,1], 'gx--',
             Weg3[0:i,0], Weg3[0:i,1],'cx--')
    plt.pause(0.4)

input()
    
#Zufällige Störungen
iterations = 10
for i in range (0,iterations):
    print("\n")
    print("Zufällige Empfänger, Iteration ",i+1," von ",iterations)
    deltay = 5 * np.random.rand(len(y),2)
    y_hut = y + deltay
    zfinal_hut = Newton(zfinal[-1], y_hut, 1e-7)
    print(zfinal_hut[-1])    
    plt.axis([0, 100, 0, 150])
    plt.plot(y[:, 0:1], y[:, 1:2], 'bH')
    plt.plot(y_hut[:, 0:1], y_hut[:, 1:2], 'b*')
    plt.plot(zfinal_hut[-1][:, 0:1], zfinal_hut[-1][:, 1:2], 'r*')
    plt.plot(zfinal[-1][:, 0:1], zfinal[-1][:, 1:2], 'r*')
    plt.pause(0.4)


input()


