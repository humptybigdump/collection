import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from sklearn.linear_model import Ridge, Lasso
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import make_pipeline
from sklearn.metrics import mean_squared_error

# Set True for Lasso, False for Ridge
LASSO = True
np.random.seed(1)
# Number of data points
data_points = 100
n_outliers = 10
data_train = int(data_points * 0.8)

# Generating synthetic data
X = np.random.uniform(-3, 3, data_points)
y = np.zeros(data_points)
param = [1, -2, 1, -.1]
for i, weight in enumerate(param):
    y += weight * X**(i)
X = X[:, np.newaxis] 

# Adding noise
y += np.random.normal(0, 1, data_points)
# Splitting data
X_train, X_test, y_train, y_test = X[:data_train], X[data_train:], y[:data_train], y[data_train:]

# Introduce outliers
outlier_indices = np.random.choice(data_train, n_outliers, replace=False)
y_train[outlier_indices] += np.random.normal(0, 10, n_outliers)

degree = 10
poly_features = PolynomialFeatures(degree=degree, include_bias=True)

# Prepare the figure and axes
fig, axes = plt.subplots(1, 2, figsize=(15, 5))

# Variables for tracking the minimum MSE and corresponding alpha
mse_min = np.inf
alpha_min = 0

# Animation update function
def update(i):
    global mse_min, alpha_min
    
    alpha_log = -5 + 0.1*i
    alpha = 10.**(alpha_log)# * 0
    model = make_pipeline(poly_features, Lasso(alpha) if LASSO else Ridge(alpha))
    model.fit(X_train, y_train)
    
    # Generate a dense set of X values for smooth plotting
    X_dense = np.linspace(X.min(), X.max(), 500)[:, np.newaxis]
    y_pred = model.predict(X_dense)
    yt_pred = model.predict(X_test)
    
    # Calculate MSE
    mse = mean_squared_error(y_test, yt_pred)
    
    # Update plots
    axes[0].clear()
    axes[0].scatter(X_train, y_train, color='red', label='Training data')
    axes[0].scatter(X_test, y_test, color='blue', label='Test data')
    axes[0].plot(X_dense, y_pred, color='green', label=f'Alpha={alpha:.5f}')
    axes[0].set_title('Fit with Current Alpha')
    axes[0].set_xlabel('X')
    axes[0].set_ylabel('y')
    axes[0].legend()
    
    axes[1].scatter(alpha_log, mse, color='lightgray')
    axes[1].set_title('MSE vs. log10(Alpha)')
    axes[1].set_xlabel('log10(Alpha)')
    axes[1].set_ylabel('MSE')
    
    if mse < mse_min:
        mse_min = mse
        alpha_min = alpha_log

# Create animation
ani = FuncAnimation(fig, update, frames=np.arange(10, 56), interval=100, repeat=False)

# Save the animation as a GIF file
#ani.save('ridge_lasso_animation.gif', writer='imagemagick')

plt.show()

print(f"The log10 of alpha with minimum MSE is: {alpha_min} with an MSE of: {mse_min}")

