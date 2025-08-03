#!/usr/bin/env python
# coding: utf-8

# ## Numpy
# Eine Bibliothek importieren
import numpy as np # Numpy: Fundamentales Paket für scientific computing

# Numpy arrays als fundamentale Datenstruktur

# Erstellung über Listen...
a = np.array([1,2,3,4,5])
print(a)
# oder Tupel
a = np.array((1,2,3,4,5))
print(a)

# Bei einer Mischung von doubles und integers wird standardmäßig zu doubles konvertiert
a = np.array([1,2,3.,4,5])
print(a) #Alle Einträge als double
print(a.dtype)

# Falls notwendig kann der Datentyp des Arrays kann aber auch manuell gesetzt werden
a = np.array([1,2,3.,4,5], dtype=np.int)
print(a)
print(a.dtype)

# Beispiele für bestimmte 1D arrays:
array_1 = np.zeros(5); print(array_1)
array_2 = np.ones(5) ; print(array_2)
array_3 = np.arange(5);print(array_3)
array_4 = np.linspace(0,4,5);print(array_4)

# Einheitsmatrix
identity = np.eye(3); print(identity)

# Zeros auch in 2D mit angabe der shape
zeros = np.zeros([3,4]);print(zeros)

# Generierung von Zufallszahlen
random = np.random.random([5,3]);print(random)

# ravel und  reshape:
a = np.arange(9);print(a)
b = a.reshape([3,3]);print(b)

# slicing
d = b[:,0]
e = b[2,:]

# Operationen werden (im Gegensatz zu Matlab) standardmäßig Elementweise durchgeführt
a = np.arange(9).reshape((3,3))
b = np.array([2,6,3])
c = np.array([4,2,5])

# Beispiel: Operationen mit Skalaren
print(a+4);print("")
print(b*3);print("")
print(c**2);print("")

# Beispiel: Operationen zwischen Arrays finden elementweise statt
print(b+c)
print(b*c)
print(b**c)
print(b/c)

# Numpy arrays sind Objekte, mit bestimmten Eigenschaften
a = np.ones(5) 
b = a    # Erzeugt keine Kopie, sondern nur einen neuen Namen für das selbe Objekt.
b *=10   # Das ändert b und a gleichermaßen ! Großer Unterschied zu Matlab
print(a) # 

# Das selbe gilt für slices:
a = np.ones([4,3])
b = a[:,2]
b *= 2
print(a)

# In funktionen kann man, statt einen neuen array zurückzugeben (Standard in Matlab) den array selbst bearbeiten, vgl c++
def manipulate(array):
    for i in range(len(array)):
        array[i] =np.exp(array[i])
a = np.eye(2).ravel()
manipulate(a)
print(a)

# Eine unabhängige Kopie erzeugen
c = a.copy()
c*=12
print(a)
print(c)

# Standard Funktionen werden auch elementweise ausgeführt
print(np.exp(a))
print(np.sin(a))
print(np.sqrt(a))
print(np.log(a+1))

# reshape und ravel
a = np.eye(4)
ravel1 = (a.ravel()) ;print(ravel1)
reshape1 = a.reshape([8,2]) ; print(reshape1)
print(a)

# reshape ändert den array nicht:
a.reshape([2,8]);print(a)

# resize ändert den array
a.resize([2,8]);print(a)

# Array transponieren
print(reshape1.T)


# ## Lineare Algebra mit numpy
# Skalarprodukt von Vektoren
a = np.arange(2,10)
print(a @ a)
print(a.dot(a))
print(np.matmul(a,a))
print(np.inner(a,a))
print(np.sum(a*a))

# Dyadisches Produkt
print(np.outer(a,a))

# Matrix-Vektor Multiplikation
A = np.arange(9).reshape([3,3])
print(A @ A)
print(np.matmul(A,A))
print(np.dot(A,A))

# Viele weitergehende Funktionen der linearen Algebra sind in der linalg Bibliothek von numpy enthalten
# https://docs.scipy.org/doc/numpy/reference/routines.linalg.html

# Für versierte Matlab user ist folgende Seite mit "Übersetzungen" hilfreich:
# https://docs.scipy.org/doc/numpy/user/numpy-for-matlab-users.html

# Beispiel: LGS lösen
A = np.array([[2,1,-1],[1,-1,-1],[2,2,1]])
b = np.array([1,3,1])
x = np.linalg.solve(A,b);
print('A =\n',A)
print('b = \n',b)
print('x = \n',x)

# Beispiel: Eigenwerte bestimmen
B = np.array( [[10.,  15  ,-12],[ 15 , -3 , -8 ],[ -12 ,-8  ,13 ]])
print('B =\n',B)
la,v = np.linalg.eig(B)
print('la =\n',la)
print('Eigenvektoren in Spalten:\n',v)
print('EV zu groesstem EW:\n',v[:,0])

# Determinante
print('Det(B) =\n',np.linalg.det(B))
# Spur
print('Spur(B)=\n',np.trace(B))

def gaussFunction(x):
    return np.exp(-x**2) - np.exp(-(x-8)**2)
x = np.linspace(-3,10,499)
y = gaussFunction(x)

# Plot Funktionen
import matplotlib.pyplot as plt
plt.plot(x,y)

# Plot Funktionen
x = np.linspace(0,2*np.pi)
y = np.sin(x)
plt.plot(x,y)
plt.show()


# In[ ]:




