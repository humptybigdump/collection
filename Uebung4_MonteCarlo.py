import numpy as np
import matplotlib.pyplot as plt
from typing import Callable, List
from scipy.optimize import newton

ABS_TOL = 1e-1
DEFAULT_SHARE = 0.95
J = 500
START_SAMPLE_SIZE = 10
value_range = [-2, 2]
spread = value_range[1] - value_range[0]
DEBUG = False


class SampleSize:

    def __init__(self):
        self.current_sample_size = START_SAMPLE_SIZE
        self.once_below_flag = False

    def next_step_size(self, share_below: bool):
        if share_below:
            if self.once_below_flag:  # finished
                self.current_sample_size = self.current_sample_size
            else: # search below current
                self.once_below_flag = True
                self.current_sample_size = int(0.2 * self.current_sample_size)
        else:
            if self.once_below_flag:
                order = (np.log10(self.current_sample_size) // 1)
                factor = self.current_sample_size / 10 ** order + 1
                self.current_sample_size = int(factor * 10 ** order)
            else:  # search exponent to 10
                self.current_sample_size = int(self.current_sample_size * 10)
        return self.current_sample_size


def mc_run(func: Callable, dim: int, current_sample_size: int):
    """Perform mc run with current_sample_size points for function func in dim dimensions."""
    rvs = np.random.rand(current_sample_size, dim) * spread + value_range[0]
    results = func(rvs)
    return np.mean(results)


def get_share_mc_runs_within_tol(dim: int, current_sample_size: int, func: Callable):
    mc_res = np.zeros(J)
    for i in range(J):
        mc_res[i] = mc_run(func, dim, current_sample_size)
    share = np.mean(np.abs(mc_res) < ABS_TOL)
    return share


def wrapper_monte_carlo_integration(dim_range: List[int], func: Callable, share_correct_to_be: float = DEFAULT_SHARE) -> np.ndarray:
    result = np.zeros_like(dim_range)
    for i, dim in enumerate(dim_range):
        if DEBUG:
            print(f'Start computation for dim {dim}')
        current_sample_size = 0
        new_step_size = SampleSize()
        while current_sample_size != new_step_size.current_sample_size:
            current_sample_size = new_step_size.current_sample_size
            if DEBUG:
                print(f'Step Size: {current_sample_size}')
            share = get_share_mc_runs_within_tol(dim=dim, func=func, current_sample_size=current_sample_size)
            new_step_size.next_step_size(share > share_correct_to_be)
        result[i] = current_sample_size
    return result


if __name__=='__main__':
    dim_range=[2]
    func = lambda x: 1 / np.sqrt(x.shape[1]) * np.sum(x, axis = 1)
    wrapper_monte_carlo_integration(dim_range, func)
