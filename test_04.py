import unittest
import numpy as np
import sys
sys.path.append('..')
from ex_04 import *


class TestEx04(unittest.TestCase):
    def test_lup(self):
        A = np.arange(9).reshape((3,3))
        A[2, 2] = 10
        l, u, p = LUP(A)
        assert np.allclose(l@u@p, A)

    def test_solve_lup(self):
        A = np.arange(9).reshape((3,3))
        A[2, 2] = 10

        x = np.random.rand(3)
        y = A@x 
        assert np.allclose(solve_LUP(A, y), x)
