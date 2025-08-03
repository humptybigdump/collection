import numpy as np
from scipy.stats import multivariate_normal
import matplotlib.pyplot as plt
import seaborn as sns


def estimate_gauss_covariance_matrix(n: int, true_covariance: np.ndarray, kind: str):
    """ Function to estimate a dim-times-dim covariance matrix based on n Gaussian random vectors with covariance matrix true_covariance

    Parameters
    ----------
    n: number of realizations of the random vectors
    true_covariance: true covariance matrix of the random vector, the size determines the dimension of the random vector
    kind: 
        'matrix': return absolute difference of estimated and true covariance matrix,
        'svd': return singular values, 
        'norm': return norm of difference between true and estimated covariance matrix

    Returns
    -------
        Depending on kind some characteristic of the covariance matrix
    """
    estimated_matrix = np.cov(multivariate_normal(mean=None, cov=true_covariance).rvs(n), rowvar=False)
    if kind == 'matrix':
        return np.abs(estimated_matrix - true_covariance)
    if kind == 'svd':
        s = np.linalg.svd(estimated_matrix, compute_uv=False, hermitian=True)
        return s
    if kind == 'norm':
        return np.linalg.norm(estimated_matrix, ord='fro')


def wrapper_covariance_estimation(n_per_sample: int, true_cov: np.ndarray, n_samples: int, kind: str, ax:plt.Axes):
    ax.clear()
    if kind == 'matrix':
        estimations = np.stack(
            [estimate_gauss_covariance_matrix(n=n_per_sample, true_covariance=true_cov, kind='matrix') for i in
             range(n_samples)])
        plot_heatmap(estimations, ax)
    if kind == 'svd':
        estimations = np.stack(
            [estimate_gauss_covariance_matrix(n=n_per_sample, true_covariance=true_cov, kind='svd') for i in
             range(n_samples)])
        plot_singular_values(estimations, ax)
    if kind == 'norm':
        estimations = np.stack(
            [estimate_gauss_covariance_matrix(n=n_per_sample, true_covariance=true_cov, kind='norm') for i in
             range(n_samples)])
        plot_norm(estimations, ax)


def plot_heatmap(estimations: np.ndarray, ax: plt.Axes):
    mean_abs_deviation = np.mean(estimations, axis=0)
    sns.heatmap(data=mean_abs_deviation, ax=ax, annot=True, cmap="YlGnBu")
    ax.set_title('Mean absolute deviation of covariance matrix components')


def plot_singular_values(estimations: np.ndarray, ax: plt.Axes):
    ax.boxplot(estimations)
    ax.set_title('Singular values')


def plot_norm(estimations, ax):
    ax.boxplot(estimations)
    ax.set_title('Frobenius norm of covariance estimate')
