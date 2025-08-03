import unittest
import numpy as np
import scipy.sparse
import sys
sys.path.append('..')
from ex_06 import *


class TestEx06(unittest.TestCase):
    def test_cg(self):
        from ex_04 import stiffness
        from ex_04 import normed_voigt
        c_nv = normed_voigt(stiffness)
        sigma_nv = [1, 0, 0, 0, 0, 0]
        eps_nv = solve_CG(c_nv, sigma_nv, maxit=10)[0]
        assert np.allclose(sigma_nv, c_nv@eps_nv)
