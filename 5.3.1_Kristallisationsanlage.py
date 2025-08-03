#!/usr/bin/env python
# coding: utf-8

# # TTP Skript Teil A: Kap. 5.3.1 Nicht lineare Prozessmodelle, Bsp. Kristallisationsanlage (wie Ü2)

# In[1]:


# Import der benötigten Module
from scipy.optimize import fsolve
import numpy as np


# # Definition der Funktion(en)

# In[2]:


# System aus MODELLGLEICHUNGEN 

def F(X_var, X_para):
     
    #Rückbenennung aller verwendeten Prozessgrößen
    [M1, x1s, x1w, x1v, M2, 
     x2w, x2v, M5, x5w, M3, 
     x3s, M6, x6w, x6v, M4, 
     x4s, x4w, x4v, M7, x7s, 
     x7w, x7v] = X_var 
    
    [M0, x0s, x0w, x0v, x2s, x5s, x5v, x3w, x3v, x6s, alpha] = X_para
     
    # Massenbilanzgleichungen (Typ BILANZ)
    res0 = M0*x0s + M7*x7s - M1*x1s
    res1 = M0*x0w + M7*x7w - M1*x1w
    res2 = M0*x0v + M7*x7v - M1*x1v
    res3 = M1*x1s - M2*x2s - M5*x5s
    res4 = M1*x1w - M2*x2w - M5*x5w
    res5 = M1*x1v - M2*x2v - M5*x5v
    res6 = M2*x2s - M3*x3s - M6*x6s
    res7 = M2*x2w - M3*x3w - M6*x6w
    res8 = M2*x2v - M3*x3v - M6*x6v
    res9 = M6*x6s - M4*x4s - M7*x7s
    res10 = M6*x6w - M4*x4w - M7*x7w
    res11 = M6*x6v - M4*x4v - M7*x7v
    
    # Definition alpha (Typ KONST.)
    res12 = alpha*M6 - M4
    
    # Schließbedingungen (Typ ZWANG)
    res13 = 1 - (x1s + x1w + x1v)
    res14 = 1 - (x2s + x2w + x2v)
    res15 = 1 - (x3s + x3w + x3v)
    res16 = 1 - (x4s + x4w + x4v)
    res17 = 1 - (x5s + x5w + x5v)
    res18 = 1 - (x6s + x6w + x6v)
    res19 = 1 - (x7s + x7w + x7v)
    
    # Split (Typ ZWANG)
    res20 = x7s - x4s
    res21 = x7w - x4w
    
    # zu minimierender Residuumsvektor
    Res = [res0, res1, res2, res3, res4, res5, 
           res6, res7, res8, res9, res10, res11, 
           res12, res13, res14, res15, res16, res17,
           res18, res19, res20, res21]
    
    return Res


# # Eingabe der Werte

# In[3]:


'''
alle Größen: 
M = Massenstrom [kg/h]
x = Massenanteil in Strom [-]
alpha = Splitfaktor [-]

einige Indizes: (Indizierung siehe Modellgleichungen im Skript)
_w = Wasser
_s = Salz
_v = Verunreinigung
'''

# Vorgabe PARAMETER (Typ ZWANG)
M0 = 1000.0
x0s = 0.5 
x0w = 0.49 
x0v = 0.01 
x2s = 0.7 
x5s = 0.0
x5v = 0.0
x3w = 0.0
x3v = 0.0
x6s = 0.5 
alpha = 0.1 
X_para = [M0, x0s, x0w, x0v, x2s, x5s, x5v, x3w, x3v, x6s, alpha]

# VARIABLEN
variablen_namen = ["M1", "x1s", "x1w" ,"x1v" ,"M2" ,
                   "x2w" ,"x2v" ,"M5" ,"x5w" ,"M3" ,
                   "x3s" ,"M6" ,"x6w" ,"x6v" ,"M4" ,
                   "x4s", "x4w","x4v" ,"M7" ,"x7s" ,
                   "x7w", "x7v"]

# SCHAETZWERTE VARIABLEN
X_var_0 = [2000, 0.33, 0.33, 0.33, 1500, 
           0.15, 0.15, 500,  0.5,  500,
           0.5,  1000, 0.25, 0.25, 100, 
           0.33, 0.33, 0.33, 900,  0.33,
           0.33, 0.33]


# # Lösung durch Aufruf der Funktion(en)

# In[4]:


# solver approximiert: res->0 für MODELLGLEICHUNGEN(X_var), Startwert X_var_0
X_var = fsolve( lambda X_var: F(X_var, X_para), X_var_0 )

# Residuumsvektor durch Einsetzen des gefundenen Lösungsvektors
res = F(X_var, X_para)
# Runden
res = np.round(res, 7)


# # Untersuchung der Ergebnisse

# In[5]:


# sauberes print out der Ergebnisse
print("\nVariablen: " + str(len(X_var)))
print("{:<10} {:<13}".format('X_var','Wert')+'\n')

for i in enumerate(X_var):
    label = variablen_namen[i[0]]
    wert = np.round(i[1], 3)
    print("{:<10} {:<13}".format(label, wert))
    
print ("\nres = \n" + str (res))

