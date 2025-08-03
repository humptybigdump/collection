import numpy as np
import pandas as pd


"""
Import the 
"""
df = pd.read_csv('windata_trainingset.csv', sep=';', header=None)
wine_data = df.values
df_test = pd.read_csv('windata_testset.csv', sep=';', header=None)
wine_data_testset = df.values

x = wine_data[1:, :-1].astype(float)
y = wine_data[1:, -1].astype(float)

x_test = wine_data_testset[1:, :-1].astype(float)
y_test = wine_data_testset[1:, -1].astype(float)




