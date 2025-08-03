import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
from network import Network
from fc_layer import FCLayer
from activation_layer import ActivationLayer
from activations import * 
from losses import * 

# Download dataset
url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx'
data = pd.read_excel(url)

# Features and target
X = data[['X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8']].values
y = data['Y1'].values

# Split the dataset
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

# Standardize the data
scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

#expand data
X_train = np.expand_dims(X_train, axis = 1)
X_test = np.expand_dims(X_test, axis = 1)
y_train = np.expand_dims(y_train, axis= (1, 2))


# Initialize network parameters
input_size = X_train.shape[2]
hidden_size = 64
output_size = 1

# model
net = Network()
net.add(FCLayer(input_size, hidden_size))
#net.add(ActivationLayer(relu, relu_prime))
net.add(FCLayer(hidden_size, 1))


# train
net.use(mse, mse_prime)
net.fit(X_train, y_train, epochs=1000, learning_rate=0.0001)

# test
out = net.predict(X_test)
