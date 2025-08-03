import numpy as np
import matplotlib.pyplot as plt
from scipy import signal
from scipy.signal import find_peaks
from scipy import fftpack
from scipy.optimize import curve_fit
import math
import subprocess
from datetime import datetime
from scipy.integrate import odeint
import os

from matplotlib import font_manager
font_manager.findSystemFonts(fontpaths=None, fontext="ttf")
font_manager.findfont("latinmodernmath") # Test with "Special Elite" too


cm = 1/2.54
from pylab import rcParams
rcParams['figure.figsize'] = 15*cm, 15*cm
plt.rcParams['font.size'] = 8
rcParams['font.family'] = 'sans-serif'
rcParams['font.family'] = ['arial']

#######################################################
# Read CSV
#######################################################
import pandas as pd

# Read the CSV file into a DataFrame
df = pd.read_csv('data.csv')

# Access data in the DataFrame using column names or indexing
t=[]
a=[]
i=0
while i<3600:
    t=np.append(t,df.iloc[i][0])
    a=np.append(a,df.iloc[i][3])
    i=i+1

#######################################################
# Prognose
#######################################################
# E_Spanplatte=.... 
# b= ...
# h= ...
# l= ...

#######################################################
# FFT
#######################################################


#######################################################
# Dämpfung
#######################################################


#######################################################
# Plot
######################################################
# fig, (ax1) = plt.subplots(1,1)
# ax1.plot(t,a, alpha=.5, color='black')
