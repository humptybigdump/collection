#!/usr/bin/env python
# coding: utf-8

# ## Aufgabe 1
import numpy as np

x = np.array([1,0,3.])
y = np.array([-1,6,0.5])

print(np.dot(x,y))
print(np.outer(x,y))
print(np.outer(y,x))
print(x*y)
print(x/y)
print(x+y)
print(x-y)

Null = np.zeros([3,4])
print(Null)
n=5;print(np.eye(n))

def random(m):
    return 1-2*np.random.random([m,m])
random(3)

A = np.arange(1,7).reshape(2,3)
print(A)
B = np.array([-1,0.5,1.5,-7,2,1,1,0.4,12]).reshape(3,3)
print(B)
C = np.array([0.1,2.4,np.sqrt(2),123,-8,2./7,0.25,1,2]).reshape(3,3)
print(C)

print(np.matmul(A,B))
print(np.matmul(B,np.transpose(A)))
print(B-C)


# ## Aufgabe 2
x = np.zeros(8)
for i in range(8):
    x[i] = i+1
print(x)

def random2(m):
    Z = np.zeros([m,m])
    for i in range(m):
        for j in range(m):
            Z[i,j] = 1-2*np.random.random()
    return Z
random2(3)


# ## Aufgabe 3
import matplotlib.pyplot as plt
x = np.linspace(0,10,200)
print(x)
y = np.exp(0.1*x)
plt.plot(x,y)

D = np.cos(A)
print(D)
