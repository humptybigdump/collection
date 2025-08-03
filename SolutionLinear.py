#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May  3 16:15:05 2021

@author: felix
"""
import numpy as np
from scipy import sparse
import matplotlib.pyplot as plt
import time
from Loesung import Problem,Quadratic,CG,Newton,MINRES2,AndersonGradient,OptimalConstantStepsizeSelector

class FiniteDifferenceStencil(object):
    def __init__(self,Nx):
        self.dims = (Nx,Nx)

    def isInner(self,i,j):
        truthvalues = [i>0,j>0,i<self.dims[0]-1,j<self.dims[1]-1]
        if False in truthvalues:
            return False
        else:
            return True
                            
    def VoxelIndex(self):
        
        self.voxelIndex = -np.ones(self.dims,dtype = np.int)
        
        counter = 0
        for i in range(self.dims[0]):
            for j in range(self.dims[1]):
                if self.isInner(i,j):
                    self.voxelIndex[i,j] = counter
                    counter+=1
        
    def stencil(self,i,j,value):
        voxels = []
        values = []
        # center
        centervoxel = self.voxelIndex[i,j]
        if centervoxel>-1:
            voxels+=[centervoxel]
            values+=[4*value]
        
        # right
        x_plus = self.voxelIndex[i+1,j]
        if x_plus>-1:
            voxels+=[x_plus]
            values+=[-1*value]
        
        # left
        x_minus = self.voxelIndex[i-1,j]
        if x_minus>-1:
            voxels+=[x_minus]
            values+=[-1*value]
        
        # up
        y_plus  = self.voxelIndex[i,j+1]
        if y_plus>-1:
            voxels+=[y_plus]
            values+=[-1*value]
        
        # down
        y_minus  = self.voxelIndex[i,j-1]
        if y_minus>-1:
            voxels+=[y_minus]
            values+=[-1*value]
        
        return voxels,values
        
    def computeLaplaceMatrix(self):
        
        row = 0
        rows = []
        cols = []
        values = []
        
        for i in range(self.dims[0]):
            for j in range(self.dims[1]):
                if self.voxelIndex[i,j]>-1:
                    
                    voxelIndices,vals = self.stencil(i,j,(self.dims[0]-1)**2)
                    rows +=len(voxelIndices)*[row]
                    cols += voxelIndices
                    values += vals
                    row+=1
        self.Matrix = sparse.csr_matrix((np.array(values),(np.array(rows),np.array(cols))))

    def combine(self,sol):
        
        solution = np.zeros(self.dims)
        for i in range(self.dims[0]):
            for j in range(self.dims[1]):
                if self.voxelIndex[i,j]==-1:
                    solution[i,j]=0
                else:
                    solution[i,j] = sol[self.voxelIndex[i,j]]
        return solution
    
    def rhs(self):
        
        rhs= np.ones([self.dims[0]-2,self.dims[1]-2])

        return rhs
    def assembleProblem(self):
        return Quadratic(self.Matrix,-self.rhs().ravel(),0.)
    def solve(self,solver):
        self.VoxelIndex()
        self.computeLaplaceMatrix()
        prob = self.assembleProblem()
        t = time.time()
        
        u,r = solver.solveProblem(prob)
        t2 = time.time()
        dt = t2-t
        print('Dauer: ',dt)
        #from scipy.sparse import linalg
        #u = linalg.spsolve(self.Matrix,self.rhs().ravel())
        
        solution = self.combine(u)
        ax = plt.axes(projection='3d')
        x = np.outer(np.linspace(0,1,self.dims[0]),np.ones(self.dims[1]))
        y = np.outer(np.ones(self.dims[0]),np.linspace(0,1,self.dims[1]))
        ax.plot_surface(x,y,solution,cmap = 'viridis')
        plt.show()
        return r,dt

if __name__ == '__main__':
    finiteDifferenceObject = FiniteDifferenceStencil(500)
    dtList = []
    iterList = []

    r1,dt1 = finiteDifferenceObject.solve(CG())

