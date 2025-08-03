import unittest
import numpy as np
import sys
sys.path.append('..')
from ex_05 import *


class TestEx05(unittest.TestCase):
    def test_posdef_sym(self):
        assert is_posdef_sym(np.eye(3))
        assert not is_posdef_sym(np.zeros((3,3)))
        A = np.random.rand(3,3)
        sym = A+A.T
        skw = A-A.T
        assert is_posdef_sym(sym+10*np.eye(3))
        assert not is_posdef_sym(skw)

    def test_cholesky(self):
        A = np.random.rand(3,3)
        posdef_sym = A+A.T + 10*np.eye(3)
        L = cholesky(posdef_sym)
        assert np.allclose(L, np.tril(L))
        assert np.allclose(L@L.T, posdef_sym)

    def test_np_matrix(self):
        a_2 = np.array([[-1, 1], [-1, 1]])
        assert np.allclose(a_2, np_matrix(2))
        a_3 = np.array([[-1, 1, 0], [-1, 2, -1], [0, -1, 1]])
        assert np.allclose(a_3, np_matrix(3))

    def test_sparse(self):
        assert np.allclose(np_matrix(10), coo_matrix(10).todense())
        assert np.allclose(np_matrix(10), csr_matrix(10).todense())
