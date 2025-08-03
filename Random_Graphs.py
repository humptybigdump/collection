import numpy as np
import math
import time

import matplotlib.pyplot as plt
import itertools as it
from copy import deepcopy

import Drawing_Graphs as DG


def ErdösRenyi(n,p):
    V = [j for j in range(n)]
    E = []
    for e in it.combinations(V,2):
        U = np.random.uniform(0,1)
        if U <= p:
            E.append(e)
    return [V,E]


def GeneralizedRG(n,w):
    l = sum(w)
    V = [j for j in range(n)]
    E = []
    for e in it.combinations(V,2):
        U = np.random.uniform(0,1)
        p = (w[e[0]]*w[e[1]]) / (l + w[e[0]]*w[e[1]])
        if U <= p:
            E.append(e)
    return [V,E]
   
    

    

def thinning(G,m):
    V,E = G
    W = [ j for j in range(m)]
    F = []
    
    for e in E:
        if (e[0] < m) and (e[1] < m):
            F.append(e)
            
    return [W,F]



def PrefAttachment(t,m,delta):
    V = [0]
    E = [(0,0) for i in range(m)]
    D = [0 for j in range(t)]
    D[0] = m
    for j in range(1,t):
        for i in range(m):
            U = np.random.uniform(0,1)
            q = 0
            l = j
            for k in range(len(V)):
                q += (D[k]+delta) / (j*(2*m+delta)+2*i+1+delta*(i+1)/m)
                if U <= q:
                    l = k
                    break  
            D[j] += 1
            D[l] += 1
            E.append((l,j))
        V.append(j)
        
    return [V,E]






    
    

if __name__ == '__main__':

    n = 100
    
    lamba = 1.5
    p = 0.03301600995
    
    l = 100
    
    m = 3
    delta = 0
    
    

    ER = ErdösRenyi(n,p)
    #DG.draw(ER,1,label='p = '+str(p))
    #DG.draw_color(ER,label='p = '+str(p))
    
    def f(k):
        if k <= 25:
            z = 1
        elif k <= 50:
            z = 2
        elif k <= 75:
            z = 3
        elif k <= 90:
            z = 5
        elif k <= 95:
            z = 10
        elif k <= 100:
            z = 25
        return z
    
    w = [ f(j) for j in range(1,n+1) ]
    
    GRG = GeneralizedRG(n,w)
    #DG.draw(GRG,1,label='W ~ U({1,...,'+str(l)+'})')
    
    N = [ j for j in range(n-1) ]
    for j in N:
        time.sleep(1)
        DG.draw_color(thinning(GRG,j+1),save=False)
    
    DG.draw_color(GRG)


    
    PA = PrefAttachment(n, m, delta) 
    #DG.draw_color(PA,label='m='+str(m)+', delta='+str(delta),multi=m)

    #N = [ j for j in range(n) ]
    #for j in N:
    #    time.sleep(1)
    #    DG.draw_color(thinning(PA,j+1),label='m='+str(m)+', delta='+str(delta),multi=m)
 
    DG.draw_color(PA,label='m='+str(m)+', delta='+str(delta),multi=m)
    
    print("ER :", len(ER[1]))
    print("GRG:", len(GRG[1]))
    print("PA :", len(PA[1]))
    

        


        
        
        
        
        
        
    
    
            
    