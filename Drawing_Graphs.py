import numpy as np
import math

import matplotlib.pyplot as plt
import itertools as it





def draw(G,type=1,save=False,label=''):
    V,E,C = Components(G)
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
    plt.xlim((-1.07,1.07))
    plt.ylim((-1.07,1.07))
    plt.axis('off')
    
    if n >= 200:
        lw = 1
        s = 1
    else:
        lw = 1
        s = 50
    
    mu = 0.03
    for e in E:  
        if e[0] != e[1]:
            plt.plot( [X[U[e[0]]],X[U[e[1]]]], [Y[U[e[0]]],Y[U[e[1]]]], color='blue', zorder=1, linewidth=lw)
        else:
            self_edge = plt.Circle(((1+mu)*X[U[e[0]]],(1+mu)*Y[U[e[0]]]),radius=mu, color='blue', fill=False)
            plt.gca().add_patch(self_edge)
    
    if len(E)>=1:
        e = E[0]
        plt.plot( [X[U[e[0]]],X[U[e[1]]]], [Y[U[e[0]]],Y[U[e[1]]]], color='blue', zorder=1, linewidth=lw, label=label)
        
    plt.scatter(X,Y,color='red', edgecolors='black', zorder=2, s=s, label='n = '+str(n))
    plt.legend(loc="upper right")
    
    if save != False:
        plt.savefig(save+'.pdf',bbox_inches='tight')
        
        
        
        

def draw_color(G,save=False,label='',multi = 1):
    V,E = G
    n = len(V)
    
    D = []
    for v in V:
        D.append(degree(G,v))
    m1 = min(D)
    m2 = max(D)
    if m2-m1 == 0:
        m = 1
    else:
        m = m2-m1
        
    P = [ (j/n)*2*math.pi for j in range(n) ]
    X = [ math.cos(j) for j in P ]
    Y = [ math.sin(j) for j in P ]
    
    plt.figure(figsize=(10,10))
    plt.xlim((-1.07,1.07))
    plt.ylim((-1.07,1.07))
    plt.axis('off')
    
    if n >= 200:
        lw = 1
        s = 1
    elif n >= 80:
        lw = 1
        s = 50
    else:
        lw = 1
        s = 80
    
    mu = 0.03
    
    if multi == 1:       
        for e in E:      
            if e[0] != e[1]:
                plt.plot( [X[e[0]],X[e[1]]], [Y[e[0]],Y[e[1]]], color='black', zorder=1, linewidth=lw)
            else:
                self_edge = plt.Circle(((1+mu)*X[e[0]],(1+mu)*Y[e[0]]),radius=mu, color='black', fill=False)
                plt.gca().add_patch(self_edge)
                
    else:
        M = {}
        for e in E:
            if e in M:
                M[e] += 1
                mul = M[e]  
            else:
                M[e] = 1
                mul = M[e]
            
            if e[0] != e[1]:
                plt.plot( [X[e[0]],X[e[1]]], [Y[e[0]],Y[e[1]]], color='black', zorder=1, linewidth=1.5*mul-1.1)
            else:
                self_edge = plt.Circle(((1+mu)*X[e[0]],(1+mu)*Y[e[0]]),radius=mu, color='black', fill=False, linewidth=1.5*mul-1.1) #linewidth=mul-0.5
                plt.gca().add_patch(self_edge)   
                
                
                
    if len(E)>=1:
        e = E[0]
        plt.plot( [X[e[0]],X[e[1]]], [Y[e[0]],Y[e[1]]], color='black', zorder=1, linewidth=lw, label=label)
    
    for j in range(n):
        plt.scatter([X[j]],[Y[j]],color=[(0,1-0.85*(D[j]-m1)/(m),0)], edgecolors='black', zorder=2, s=s)
    plt.scatter([],[],color=[(0,1,0)], edgecolors='black', zorder=2, s=s, label='n = '+str(n))    
    plt.legend(loc="upper right")
    
    if save != False:
        plt.savefig(save+'.pdf',bbox_inches='tight')
    plt.show()
    plt.close()





def Components(G):
    V,E = G
    n = len(V)
    
    H = [0 for j in range(n)]
    Z = []
    for k in range(n):
        for ZK in Z:
            for l in ZK:
                if l==k:
                    H[k]=1
        if H[k]==0:
            Z.append(Component(G, k))
            
    Z.sort(key=lambda x:len(x), reverse=True)
    W = []
    for ZK in Z:
        for l in ZK:
            W.append(l)
    
    return [W,E,Z]
    


def Component(G,k):
    V,E = G
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



def degree(G,k):
    d = 0
    for e in G[1]:
        if (e[0] == k) ^ (e[1] == k):
            d += 1
        elif (e[0] == k) and (e[1] ==k):
            d += 2
    return d






        

    
        
        
        
        