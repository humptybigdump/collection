from typing import Union, List

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats


def sample_spherical(n: int, ndim: int) -> np.array:
    """ Generate sample of spherical distribution

    Parameters
    ----------
    n: number of points
    ndim: dimension of points

    Returns
    -------
    np.ndarray with sample
    """
    pts = stats.norm.rvs(size=(ndim, n))
    pts = pts / np.linalg.norm(pts, axis=0) * np.sqrt(ndim)
    return pts.T


def plot_kde_margin_spherical(n: int, ndim: int, ax: plt.Axes) -> plt.Axes:
    """ Plot kde of the first dimension of n realisations of ndim-dimensional spherical random variable

    Parameters
    ----------
    n: number of points
    ndim: dimension of points

    Returns
    -------
    plt.Axes with kde plot
    """
    rvs = sample_spherical(n=n, ndim=ndim)
    ax = sns.kdeplot(rvs[:, 0], ax=ax, clip=[-np.sqrt(ndim), np.sqrt(ndim)])
    ax.set_title(f'Kerndichteschätzung des eindimensionalen Randes \n auf Basis von {n} Realisationen in {ndim} Dimensionen')
    return ax
