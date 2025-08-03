import unittest
import numpy as np
import sys
sys.path.append('..')
from ex_07 import *
from scipy.sparse import isspmatrix

class TestEx07(unittest.TestCase):
    def test_parabola(self):
        test_parameters = [(1, 1, 1), (0, 1, 0), (0, 0, 2)]
        for (a, b, c) in test_parameters:
            test_parabola = Parabola(a, b, c)
            assert np.allclose(test_parabola.value(np.array([-1, 0, 1])),
                               np.array([a-b+c, c, a+b+c]))
            assert np.allclose(test_parabola.diff(np.array([-1, 0])),
                               np.array([-2*a+b, b]))

    def test_cos(self):
        test_parameters = [1, 2*np.pi]
        for a in test_parameters:
            test_cos = Cos(a)
            assert(np.allclose(test_cos.value(np.array([0, 2*np.pi/a, np.pi/(2*a)])),
                               np.array([1, 1, 0])))
            assert(np.allclose(test_cos.diff(np.array([0, 2*np.pi/a, np.pi/(2*a)])),
                               np.array([0, 0, -a])))

    def test_exp(self):
        test_parameters = [1, -1]
        for a in test_parameters:
            test_exp = Exp(a)
            assert(np.allclose(test_exp.value(np.array([0, 1])),
                np.array([1, np.e**a])))
            assert(np.allclose(test_exp.diff(np.array([0, 1])),
                np.array([a, a*np.e**a])))


    def test_grid_init(self):
        x0 = np.random.rand()
        h = np.random.rand()
        N = np.random.randint(10, 1000)
        test_grid = Grid1D(x0, h, N)
        assert test_grid.h == h
        assert test_grid.N == N
        assert len(test_grid.x) == N
        assert test_grid.x[0] == x0
        assert np.allclose(test_grid.x[1]-x0, h)

    def test_grid_backward(self):
        x0 = np.random.rand()
        h = np.random.rand()
        N = np.random.randint(10, 1000)
        test_grid = Grid1D(x0, h, N)
        backward = test_grid.backward_matrix()
        for f in [f_1, f_2, f_3]:
            assert np.allclose(backward@f.value(test_grid.x), 
                               backward_euler(test_grid.x, f))

    def test_grid_forward(self):
        x0 = np.random.rand()
        h = np.random.rand()
        N = np.random.randint(10, 1000)
        test_grid = Grid1D(x0, h, N)
        forward = test_grid.forward_matrix()
        for f in [f_1, f_2, f_3]:
            assert np.allclose(forward@f.value(test_grid.x), 
                               forward_euler(test_grid.x, f))

    def test_grid_midpoint(self):
        x0 = np.random.rand()
        h = np.random.rand()
        N = np.random.randint(10, 1000)
        test_grid = Grid1D(x0, h, N)
        midpoint = test_grid.midpoint_matrix()
        for f in [f_1, f_2, f_3]:
            grid_solution = midpoint@f.value(test_grid.x)
            func_solution = midpoint_rule(test_grid.x, f)
            assert np.allclose(grid_solution, func_solution)