# Computation of persistent homology and cycle representatives with Ripser
# Lecture course "Topological Data Analysis"
# Lecture 29
# KIT  WS 20/21

# Import Python subprocess package

import subprocess

# Compute persistent homology and cycle representatives with Ripser

subprocess.run(['./ripser-representatives', 'distance_matrix.csv'])
