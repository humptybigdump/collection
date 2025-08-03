import unittest
import numpy as np
import sys
sys.path.append('..')
from ex_08 import *

def stiffness_test_helper(x0, springs, stiffness):
    test_problem = SpringProblem1D(x0, springs, [(0,0)], [(len(x0)-1,1)])
    print(test_problem.reduced_system())
    x_solution = test_problem.solve_for_pos()
    assert np.isclose(1/(x_solution[-1]-x0[-1]), stiffness)

class TestSprings(unittest.TestCase):

    def test_single_spring(self):
        x0 = np.array([0, 1])
        springs = [(0, 1, .5)]
        stiffness = .5
        stiffness_test_helper(x0, springs, stiffness)


    def test_serial_springs(self):
        x0 = np.array([0, 0.5, 1])
        springs = [(0, 1, 1), (1,2,2)]
        stiffness = 2/3
        stiffness_test_helper(x0, springs, stiffness)

    def test_parallel_springs(self):
        x0 = np.array([0, 1])
        springs = [(0, 1, .5), (0, 1, 1)]
        stiffness = 1.5
        stiffness_test_helper(x0, springs, stiffness)
