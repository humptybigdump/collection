import numpy as np
import math

import matplotlib.pyplot as plt
import itertools as it
from copy import deepcopy



def ErdösRenyi(n,p):
    V = [j for j in range(n)]
    E = []
    for e in it.combinations(V,2):
        U = np.random.uniform(0,1)
        if U <= p:
            E.append(e)
    return [V,E,None]



def Components(ER):
    V,E,N = ER
    n = len(V)
    
    H = [0 for j in range(n)]
    Z = []
    for k in range(n):
        for ZK in Z:
            for l in ZK:
                if l==k:
                    H[k]=1
        if H[k]==0:
            Z.append(Component(ER, k))
            
    Z.sort(key=lambda x:len(x), reverse=True)
    W = []
    for ZK in Z:
        for l in ZK:
            W.append(l)
    
    return [W,E,Z]
    

def Component(ER,k):
    V,E,p = ER
    n = len(V)
    
    H = [0 for j in range(n)]
    H[k] = 1
    
    Z = [[k]]
    while 0 in H:
        ZK = []
        for j in range(n):
            if H[j] == 0:
                for l in Z[-1]:
                    if H[j] == 0:
                        if (j,l) in E or (l,j) in E:
                            ZK.append(j)
                            H[j] = 1
        if len(ZK)>0:
            Z.append(ZK)
        if ZK == []:
            H = [1 for j in range(n)]
            
    C = []
    for ZK in Z:
        for l in ZK:
            C.append(l)
                           
    return C




def draw(ER,type=1,save=False):
    V,E,C = ER
    n = len(V)
    
    if type == 1:
        P = [ (j/n)*2*math.pi for j in range(n) ]
        X = [ math.cos(j) for j in P ]
        Y = [ math.sin(j) for j in P ]
    
    if type == 2:
        m = math.ceil(math.sqrt(n))
        L = [ (j+0.5)*(2/m)-1 for j in range(m) ]
        X = m*L
        Y = []
        for j in range(m):
            L = [ (j+0.5)*(2/m)-1 for i in range(m) ]
            Y = Y + L
        for i in range(m**2-n):
            X.pop()
            Y.pop()   

    U = []
    for j in range(n):
        for i in range(n):
            if V[i]==j:
                U.append(i)
                break

    plt.figure(figsize=(10,10))
    plt.xlim((-1.03,1.03))
    plt.ylim((-1.03,1.03))
    plt.axis('off')
    
    if n >= 200:
        lw = 1
        s = 1
    else:
        lw = 1.5
        s = 20
    
    for e in E:
        plt.plot( [X[U[e[0]]],X[U[e[1]]]], [Y[U[e[0]]],Y[U[e[1]]]], color='dodgerblue', zorder=1, linewidth=lw)
    
    if len(E)>=1:
        e = E[0]
        plt.plot( [X[U[e[0]]],X[U[e[1]]]], [Y[U[e[0]]],Y[U[e[1]]]], color='dodgerblue', zorder=1, linewidth=lw, label='p = '+str(p))
        
    plt.scatter(X,Y,color='red', zorder=2, s=s, label='n = '+str(n))
    plt.legend(loc="upper right")
    #plt.title("Realisation of an Erdös-Renyi graph")
    
    if save != False:
        plt.savefig(save+'.pdf',bbox_inches='tight')
   
    
   
    
    
    
    
    
    

if __name__ == '__main__':

    n = 1000
    lamba = 1.5
    p = lamba / n
    
    t = 10**3
    simulate = True
    save = False
    
    ER = ErdösRenyi(n,p)
    
    ER = Components(ER)
    
    print("\n number of edges           =", len(ER[1]))
    print("\n number of components      =", len(ER[2]))
    print("\n size of biggest component =", len(ER[2][0]))
    
    
    
    if save == True:
        draw(ER,1,"ER_circle")
        draw(ER,2,"ER_rectangle")
    else:
        draw(ER,1)
        draw(ER,2)
    
        
    
    if simulate == True:
     
        numC = []
        bigC = []
    
        print("\n\n")
    
        for i in range(t):
            ER = ErdösRenyi(n,p)      
            ER = Components(ER)
            numC.append(len(ER[2]))
            bigC.append(len(ER[2][0]))
            if (i+1) % 10 == 0:
                print(i+1)
            
        print("\n\n")

        print("\n Expectation number of components:     ", np.mean(numC))
        print("\n Expectation size of biggest component:", np.mean(bigC))
        print("\n Variance number of components:        ", np.var(numC))
        print("\n Variance size of biggest component:   ", np.var(bigC))
        
        
        
        
        
        
    
    
            
    