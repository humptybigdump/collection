from typing import List

import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
import seaborn as sns


def sample_vector_length(n: int, ndim: int, rv: stats.rv_continuous) -> np.array:
    """ Compute sample vector length with size n of ndim-dimensional distribution for rv"""
    rvs = rv.rvs(size=(n, ndim))
    norms = np.sqrt(np.sum(rvs ** 2, axis=1))
    return norms


def plot_kde_vector_length(rv_list: List[stats.rv_continuous], rv_string: List[str], n: int, ndim: int, ax:plt.Axes):
    """Plot sample kde vector length with n samples in ndim dimensions into ax"""
    norms = dict()
    for i, rv in enumerate(rv_list):
        norms[rv_string[i]] = sample_vector_length(n, ndim, rv)
    sns.kdeplot(data=norms, ax=ax)
    return ax