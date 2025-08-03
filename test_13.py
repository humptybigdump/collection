import unittest
import numpy as np
import sys
sys.path.append('..')
from ex_13 import *
from functions import *


class TestEx13(unittest.TestCase):
    def test_nc_points(self):
        assert np.allclose(get_nc_points(4),np.linspace(-1,1,4))
        assert np.allclose(get_nc_points(9),np.linspace(-1,1,9))
    
    def test_gauss_points(self):
        x = np.array(np.sqrt(3/5)*np.array([-1,0,1]))
        assert np.allclose(np.sort(get_gauss_points(3)),x)
        x1 = np.sqrt(3/7+2/7*np.sqrt(6/5))
        x2 = np.sqrt(3/7-2/7*np.sqrt(6/5))
        x = np.array([-x1,-x2,x2,x1])
        assert np.allclose(np.sort(get_gauss_points(4)),x)
    
    def test_weights(self):
        x = np.linspace(-1,1,5)
        w = np.array([7,32,12,32,7])/45
        assert np.allclose(get_weights(x),w)
        x1 = np.sqrt(3/7+2/7*np.sqrt(6/5))
        x2 = np.sqrt(3/7-2/7*np.sqrt(6/5))
        x = np.array([-x1,-x2,x2,x1])
        w1 = (18-np.sqrt(30))/36
        w2 = (18+np.sqrt(30))/36
        w = np.array([w1,w2,w2,w1])
        assert np.allclose(get_weights(np.sort(x)),w)
    
    def test_transform_points(self):
        int_points = np.array([-0.5,0,0.75])
        assert np.allclose(transform_int_points(int_points,-2,5),np.array([-0.25,1.5,4.125]))
    
    def test_get_integral_1D(self):
        x = 2*np.array(np.sqrt(3/5)*np.array([-1,0,1]))
        w = np.array([5,8,5])/9
        func = Exp(2,3)
        values = func.value(x)
        num_int = 2*np.sum(w*values)
        assert np.isclose(get_integral_1D(func,x,w,-2,2),num_int)        
    
    def test_transform_points_2D(self):
        points = np.array([[-1.5,3],[-1.5,5],[1.5,3],[1.5,5]])
        int_points = np.array([-0.5,0.5])
        assert np.allclose(transform_int_points_2D(int_points,np.array([-3,2]),np.array([3,6])),points)
    
    def test_weights_2D(self):
        w = np.array([25,40,25,40,64,40,25,40,25])/81
        int_points = np.sqrt(3/5)*np.array([-1,0,1])
        assert np.allclose(get_weights_2D(int_points),w)
    
    def test_get_integral_2D(self):
        P = MultiparameterPolynomial(np.array([[2,1],[3,0]]))
        int_points = np.array([[-1,2],[-1,6],[7,2],[7,6]])
        w = np.array([1,1,1,1])
        low = np.array([-1,2])
        high = np.array([7,6])
        assert np.isclose(get_integral_2D(P,int_points,w,low,high),1248)
    