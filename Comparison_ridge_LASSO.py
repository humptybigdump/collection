import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error

# Number of data points
data_points = 100
n_outliers = 5

# Generating synthetic data
np.random.seed(0)
X = np.random.uniform(-4, 4, data_points)
y = np.zeros(data_points)
param = [1, 2, -1]
for i, weight in enumerate(param):
    y += weight * X**(i)
y_noisy = y + np.random.normal(0, 2, data_points)  # Adding noise
X = X[:, np.newaxis]

X_train, X_test, y_train, y_test = train_test_split(X, y_noisy, test_size=0.2, random_state=0)
outlier_indices = np.random.choice(y_train.size, n_outliers, replace=False)
y_train[outlier_indices] += np.random.normal(0, 5, n_outliers)

# Preparing Polynomial Features
degree = 10  # We use 3rd order polynomial features for regression
poly_features = PolynomialFeatures(degree=degree, include_bias=True)

# Initializing models
linear_model = make_pipeline(poly_features, LinearRegression())
ridge_model = make_pipeline(poly_features, Ridge(alpha=3))
lasso_model = make_pipeline(poly_features, Lasso(alpha=0.1))

# Prepare the figure and axes
fig, axes = plt.subplots(1, 4, figsize=(20, 5))
titles = ['Linear Regression', 'Ridge Regression', 'Lasso Regression']

# Animation update function
def update(i):
    # Use at least 10 data points for fitting
    if i < 20:
        return

    # Clear previous plots
    for ax in axes[:-1]:
        ax.clear()

    # Update the models with incremental data points
    linear_model.fit(X_train[:i], y_train[:i])
    ridge_model.fit(X_train[:i], y_train[:i])
    lasso_model.fit(X_train[:i], y_train[:i])

    # Generate a dense set of X values for smooth plotting
    X_dense = np.linspace(X_train[:i].min(), X[:i].max(), 500)[:, np.newaxis]

    # Predictions
    y_linear_pred = linear_model.predict(X_dense)
    y_ridge_pred = ridge_model.predict(X_dense)
    y_lasso_pred = lasso_model.predict(X_dense)
    
    yt_linear_pred = linear_model.predict(X_test)
    yt_ridge_pred = ridge_model.predict(X_test)
    yt_lasso_pred = lasso_model.predict(X_test)
    
    # Calculate L2 loss (mean squared error)
    mse_linear = mean_squared_error(y_test, yt_linear_pred)
    mse_ridge = mean_squared_error(y_test, yt_ridge_pred)
    mse_lasso = mean_squared_error(y_test, yt_lasso_pred)

    # Plot new predictions and data
    for ax, pred, title, color in zip(axes, [y_linear_pred, y_ridge_pred, y_lasso_pred], titles, ['blue', 'red', 'green']):
        ax.scatter(X[:i], y_noisy[:i], color='gray')
        ax.scatter(X[:i], y[:i], color='lightgray')
        ax.plot(X_dense, pred, color=color)
        ax.set_title(title)
        ax.set_xlabel('X')
        ax.set_ylabel('y')
    axes[3].set_xlabel('Number of data')
    axes[3].set_ylabel('L2 Loss (Mean Squared Error)')
    axes[3].set_title('Performance on Test Data')

    for mse, c in zip([mse_linear, mse_ridge, mse_lasso], ['blue', 'red', 'green']):
        axes[3].scatter(i, mse, color=c)
       
# Create animation
ani = FuncAnimation(fig, update, frames=np.arange(2, len(X)), interval=10, repeat=False)

# Display the animation
plt.show()





