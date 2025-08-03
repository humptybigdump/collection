# Install necessary libraries
# !pip install numpy pandas matplotlib scikit-learn

# Import libraries
import numpy as np
import pandas as pd
from sklearn.model_selection import ParameterGrid
import matplotlib.pyplot as plt


def generate_experiment_design(parameters, levels, design_type='full'):
    """
    Generate an experimental design using the specified parameters and levels.
    
    Parameters:
    - parameters (list): List of parameters to be varied.
    - levels (list of lists): Levels for each parameter.
    - design_type (str): Type of experimental design ('full', 'fractional', 'custom').
    
    Returns:
    - pd.DataFrame: Experimental design with combinations of parameter values.
    """
    if design_type == 'full':
        design = pd.DataFrame(list(ParameterGrid(dict(zip(parameters, levels)))))
    elif design_type == 'fractional':
        # You can implement fractional factorial design here if needed
        # ...
        pass
    elif design_type == 'custom':
        # You can implement custom design here if needed
        # ...
        pass
    else:
        raise ValueError("Invalid design type. Choose 'full', 'fractional', or 'custom'.")
    
    return design


def simulate_enzymatic_reaction(parameters):
    """
    Simulate the enzymatic reaction based on the specified parameters.
    
    Parameters:
    - parameters (pd.Series): Series containing parameter values.
    
    Returns:
    - float: Simulated reaction rate or product yield.
    """
    # Implement your simulation model here using the given parameters
    # ...
    # Example: 
    reaction_rate = 0.45
    reaction_rate = parameters['enzyme_concentration'] * parameters['substrate_concentration']
    product_yield = reaction_rate * parameters['reaction_time']
    return product_yield
