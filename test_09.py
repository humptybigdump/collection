#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 12 09:44:46 2021

@author: max
"""
import unittest
import numpy as np
import sys
sys.path.append('..')
from ex_09 import *

class TestLinElement1D(unittest.TestCase):
    
    def N_check(self,element,x):
        result = np.zeros((1,2))
        result[0,0] = -x/element.l+1
        result[0,1] =  x/element.l
        return result
    
    def B_check(self,element):
        result = np.zeros((1,2))
        result[0,0] = -1/element.l
        result[0,1] =  1/element.l
        return result
    
    def K_check(self, element):
        # integration performed on paper.
        result = np.array([[1, -1],[-1, 1]], dtype=float)
        result *= element.stiffness/element.l
        return result
    
    def test_N(self):
        stiffness = 10
        pos = [0, 1]
        test_element = LinElement1D(stiffness, [0,1], pos)
        x = 0.5
        assert np.allclose(test_element.get_N(x),self.N_check(test_element,x))
    
    def test_B(self):
        stiffness = 10
        pos = [0, 1]
        test_element = LinElement1D(stiffness, [0,1], pos)
        assert np.allclose(test_element.get_B(),self.B_check(test_element))
        
    def test_K(self):
        stiffness = 10
        pos = [0, 1]
        test_element = LinElement1D(stiffness, [0,1], pos)
        assert np.allclose(test_element.K, self.K_check(test_element))


class TestFEMGrid1D(unittest.TestCase):
    
    def test_assembly(self):
        nodepos = np.linspace(0, 1, 10)
        elements = []
        for i in range(0, 10, 2):
            elements.append(LinElement1D(i+1, [i, i+1], nodepos))
        grid = FEMGrid1D(nodepos, elements, [], [])
        full_matrix = grid.assemble_full_matrix()
        for element in elements:
            nodes = element.nodes
            assert np.allclose(full_matrix[nodes[0], nodes[0]], element.K[0,0])
            assert np.allclose(full_matrix[nodes[0], nodes[1]], element.K[0,1])
            assert np.allclose(full_matrix[nodes[1], nodes[0]], element.K[1,0])
            assert np.allclose(full_matrix[nodes[1], nodes[1]], element.K[1,1])


def fem_stiffness_linear(node_positions):
    element_nodes = []
    element_node_positions = []
    for i in range(len(node_positions)-1):
        element_nodes.append([i, i+1])
        element_node_positions.append([node_positions[i],node_positions[i+1]])
    element_nodes = np.array(element_nodes)
    element_node_positions = np.array(element_node_positions)
    stiffnesses = np.array([1]*len(element_nodes))
    elements = [LinElement1D(stiffness, nodes, positions) 
                for stiffness, nodes, positions in zip(stiffnesses, element_nodes, element_node_positions)]
    displacements = [(0, 0)]
    forces = [(len(node_positions)-1, 1)]
    fem_problem = FEMGrid1D(node_positions, elements, displacements, forces)
    x_solution = fem_problem.solve_for_pos()
    return x_solution


class TestFemSolution(unittest.TestCase):
    
    def test_linear(self):
        ref = [0,1,2,4]
        assert np.allclose(fem_stiffness_linear([0,0.5,1,2]), ref)
