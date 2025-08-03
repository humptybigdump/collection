import numpy as np
from scipy.stats import bernoulli
import matplotlib.pyplot as plt
import seaborn as sns


def generate_sbm(n_half: int, p: float, q: float, relax: bool):
    """ Return adjacency matrices for realisation of stochastic block model with parameters n=2*n_half,
    p (prob for edge within group) and q(prob for edge between groups).
    If relax=True the relaxed stochastic block model with self-adjacent nodes is used."""

    n = n_half * 2

    ## Version 1 with loop
    adj_mat = np.zeros((n, n))
    for i in range(0, n):
        for j in range(i + 1, n):
            if (i - (n - 1) / 2) * (j - (n - 1) / 2) > 0:
                edge = bernoulli.rvs(p)
            else:
                edge = bernoulli.rvs(q)
            adj_mat[i, j] = edge
            adj_mat[j, i] = edge

    if relax:
        for i in range(0, n):
            adj_mat[i, i] = bernoulli.rvs(p)

    ## Version 2 without loop

    # Q = np.ones((n_half, n_half)) * q
    # rvs = np.random.rand(n, n)
    #
    # if relax:
    #     P_tilde = np.ones((n_half, n_half)) * p
    #     D_tilde = np.column_stack([np.row_stack([P_tilde, Q]), np.row_stack([Q, P_tilde])])
    #     adj_mat = rvs < D_tilde
    # else:
    #     P = np.ones((n_half, n_half)) * p
    #     np.fill_diagonal(P, 0)
    #     D = np.column_stack([np.row_stack([P, Q]), np.row_stack([Q, P])])
    #     adj_mat = rvs < D

    return adj_mat


def spectral_clustering(adj_mat: np.ndarray):
    """Perform spectral clustering on the adjacency matrix adj_mat"""
    eig_values, eig_vector = eig_sorted(adj_mat)
    clustering = (np.sign(eig_vector[:, 1]) + 1) / 2
    return clustering


def n_nodes_right_classification(clustering: np.ndarray):
    n_half = int(len(clustering) / 2)
    true_classification = np.concatenate([np.zeros(n_half), np.ones(n_half)], axis=0)
    n_nodes_classified = np.sum(clustering == true_classification)
    return np.max([n_nodes_classified, 2 * n_half - n_nodes_classified]) # It is ok if 0s and 1s are switched


def wrapper_smb(n_half: int, p: float, q: float, n_samples: int, relax: True, kind: list[str]) -> np.ndarray:
    """Perform n_samples a classification of a n_half*2, p, q stochastic block model.
    If relax=True nodes can be self-adjacent."""
    if 'n_nodes' in kind:
        n_nodes_right = np.zeros(n_samples)
        for i in range(0, n_samples):
            adj_mat = generate_sbm(n_half=n_half, p=p, q=q, relax=relax)
            clustering = spectral_clustering(adj_mat)
            n_nodes_right[i] = n_nodes_right_classification(clustering=clustering)
        return n_nodes_right


def plot_smb(n_half: int, p: float, q:float, n_samples: int, relax: True, kind: list[str], ax:plt.Axes):
    """Wrapper for Jupyter Plot"""
    if 'n_nodes' in kind:
        n_nodes_right = wrapper_smb(n_half=n_half, p=p, q=q, n_samples=n_samples, relax=relax, kind=kind)
        plt.hist(n_nodes_right)
        ax.set_xlabel('# nodes classified right')
        return ax


#### Helpers

def eig_sorted(matrix: np.array):
    """Perform eigenvalue decomposition and sort by eigenvalues"""
    eig_value, eig_vector = np.linalg.eig(matrix)
    sort_index = np.argsort(-eig_value)
    eig_value = eig_value[sort_index]
    eig_vector = eig_vector[:, sort_index]
    return eig_value, eig_vector


if __name__ == '__main__':
    print(wrapper_smb(n_half=100, p=0.8, q=0.2, n_samples=10, relax=False, kind=['n_nodes']))
