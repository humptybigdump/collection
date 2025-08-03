import unittest
import numpy as np
import sys
sys.path.append('..')
from ex_03 import *


class TestEx03(unittest.TestCase):
    def test_sym(self):
        A = np.arange(9).reshape((3, 3))
        A_sym = [[0, 2, 4], [2, 4, 6], [4, 6, 8]]
        assert np.allclose(sym(A), A_sym)

    def test_skw(self):
        A = np.arange(9).reshape((3, 3))
        A_skw = [[0, -1, -2], [1, 0, -1], [2, 1, 0]]
        assert np.allclose(skw(A), A_skw)
        random_A = np.random.rand(10, 10)
        assert np.isclose(np.trace(skw(random_A)), 0)

    def test_normed_voigt_order_2(self):
        A = sym(np.arange(9).reshape((3, 3)))
        A_nv = normed_voigt_order_2(A)
        assert np.isclose(np.linalg.norm(A_nv), np.linalg.norm(A))
        assert np.allclose(A_nv, [0, 4, 8, np.sqrt(2) * 6, np.sqrt(2) * 4, np.sqrt(2) * 2])

    def test_stress_voigt_order_2(self):
        A = sym(np.arange(9).reshape((3, 3)))
        A_sigmav = stress_voigt_order_2(A)
        assert np.isclose(np.linalg.norm(A_sigmav), np.linalg.norm(np.triu(A)))
        assert np.allclose(A_sigmav, [0, 4, 8, 6, 4, 2])

    def test_strain_voigt_order_2(self):
        A = sym(np.arange(9).reshape((3, 3)))
        A_epsv = strain_voigt_order_2(A)
        comp_norm = np.sqrt(np.sum(A ** 2 + (A - np.diag(np.diag(A))) ** 2))
        assert np.isclose(np.linalg.norm(A_epsv), comp_norm)
        assert np.allclose(A_epsv, [0, 4, 8, 12, 8, 4])

    def test_normed_voigt_order_4(self):
        A = sym(np.arange(9).reshape((3, 3)))
        A_nv = normed_voigt_order_2(A)
        A_comp = np.tensordot(A_nv, A_nv, axes=0)
        A = np.tensordot(A, A, axes=0)
        A_nv = normed_voigt_order_4(A)
        assert np.isclose(np.linalg.norm(A_nv), np.linalg.norm(A))
        assert np.allclose(A_nv, A_comp)

    def test_stress_voigt_order_4(self):
        A = sym(np.arange(9).reshape((3, 3)))
        A_sigmav = stress_voigt_order_2(A)
        A_comp = np.tensordot(A_sigmav, A_sigmav, axes=0)
        comp_norm = np.linalg.norm(np.triu(A))
        A = np.tensordot(A, A, axes=0)
        A_sigmav = stress_voigt_order_4(A)
        assert np.isclose(np.linalg.norm(A_sigmav), comp_norm ** 2)
        assert np.allclose(A_sigmav, A_comp)

    def test_strain_voigt_order_4(self):
        A = sym(np.arange(9).reshape((3, 3)))
        A_epsv = strain_voigt_order_2(A)
        A_comp = np.tensordot(A_epsv, A_epsv, axes=0)
        comp_norm = np.sqrt(np.sum(A ** 2 + (A - np.diag(np.diag(A))) ** 2))
        A = np.tensordot(A, A, axes=0)
        A_epsv = strain_voigt_order_4(A)
        assert np.isclose(np.linalg.norm(A_epsv), comp_norm ** 2)
        assert np.allclose(A_epsv, A_comp)
