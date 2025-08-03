import numpy as np


class AbstractStepsize:
    """Collect stepsize types for gradient schemes"""

    def __init__(self, function) -> None:
        self.function = function

    def get_next_iterate(self, x):
        return x-self.alpha*self.function.gradient(x)

    def update_stepsize(self, x):
        raise Exception('Pls dont')

    @staticmethod
    def select_from_name(function, stepsize_name, fixed_stepsize=None):
        """
        Returns an object of a child class of AbstractStepsize. 

        staticmethods can be called from the class itself (i.e., we don't need an object of type AbstractStepsize to use the method AbstractStepsize.select_from_name()

        """
        if stepsize_name == 'ConstantStepsize':
            return ConstantStepsize(function, fixed_stepsize=fixed_stepsize)
        elif stepsize_name == 'FullRelaxationStepsize':
            return FullRelaxationStepsize(function)
        elif stepsize_name == 'MalitskyMishchenko':
            return MalitskyMishchenko(function)
        elif stepsize_name == 'HeavyBall':
            return HeavyBall(function)


class ConstantStepsize(AbstractStepsize):
    """Constant stepsize, possibly using L and mu"""

    def __init__(self, function, fixed_stepsize=None) -> None:
        super().__init__(function)
        self.fixed_stepsize = fixed_stepsize
        if self.fixed_stepsize is None:
            if self.function.has_L:
                if self.function.has_mu:
                    self.alpha = 2./(self.function.mu+self.function.L)
                    print(
                        f'\tEvaluating using optimal stepsize choice L:{self.function.L}, mu: {self.function.mu}')
                else:
                    self.alpha = 1./self.function.L

        elif isinstance(self.fixed_stepsize, float):
            self.alpha = self.fixed_stepsize
        else:
            raise Exception
        print(f'\tConstant stepsize: {self.alpha}')

    def update_stepsize(self, x):

        return self.alpha


class FullRelaxationStepsize(ConstantStepsize):
    """Full relaxation, will only work for the quadratic function"""

    def __init__(self, function) -> None:
        assert function.has_Q, 'Full relaxation only possible for quadratic problems!'
        super().__init__(function)

    def update_stepsize(self, x):
        grad = self.function.gradient(x)
        self.alpha = np.linalg.norm(
            grad)**2/(np.dot(grad, self.function.Q@grad))
        return self.alpha


class MalitskyMishchenko(AbstractStepsize):
    """Optimized step sizes without knowledge of neither L or mu"""

    def __init__(self, function) -> None:
        super().__init__(function)
        self.alpha_prev = 1e-16
        self.alpha = self.alpha_prev
        self.theta_prev = 1e16
        self.k = 0

    def update_stepsize(self, x):
        if self.k == 0:
            self.x_prev = x
            self.grad_prev = self.function.gradient(x)

        else:
            grad = self.function.gradient(x)
            self.alpha = np.min([np.sqrt(1+self.theta_prev)*self.alpha_prev,
                                 np.linalg.norm(x-self.x_prev)/(2*np.linalg.norm(grad-self.grad_prev))])
            self.x_prev = x
            self.grad_prev = grad
            self.theta_prev = self.alpha/self.alpha_prev
            self.alpha_prev = np.copy(self.alpha)
        self.k += 1
        return self.alpha


class HeavyBall(AbstractStepsize):
    """Heavy ball method"""

    def __init__(self, function) -> None:
        super().__init__(function)
        self.k = 0
        assert self.function.has_mu and self.function.has_L, 'L and mu are needed for Heavy Ball to work!'
        sqL = np.sqrt(self.function.L)
        sqmu = np.sqrt(self.function.mu)
        self.alpha = (2/(sqL+sqmu))**2
        self.beta = ((sqL-sqmu)/(sqL+sqmu))**2

    def get_next_iterate(self, x):
        if self.k == 0:
            x_new = super().get_next_iterate(x)
            self.x_prev = np.copy(x)
        else:
            x_new = x-self.alpha * \
                self.function.gradient(x)+self.beta*(x-self.x_prev)
            self.x_prev = np.copy(x)
        self.k += 1
        return x_new

    def update_stepsize(self, x):
        return self.alpha


class AbstractSolver:
    """An abstract Class for various kinds of solvers to inherit from"""

    def __init__(self, function, stepsize, tol, maxit) -> None:
        self.function = function
        self.tol = tol
        self.maxit = maxit
        self.stepsize = stepsize


class GradientDescent(AbstractSolver):
    """Gradient descent. This is always used, no matter what kind of step size is selected."""

    def __init__(self, function, stepsize, tol=1e-6, maxit=10000) -> None:
        super().__init__(function, stepsize, tol, maxit)

    def get_residual(self, x):
        return np.linalg.norm(self.function.gradient(x))

    def solve(self, x0, fixed_alpha=None, list_x=False):
        k = 0
        res = 1
        res_list = []
        alpha_list = []
        x_list = []
        x = np.copy(x0)
        print(f"Solving...")
        while (res > self.tol) and (k < self.maxit):
            alpha = self.stepsize.update_stepsize(x)
            x = self.stepsize.get_next_iterate(x)
            res = self.get_residual(x)
            res_list.append(res)
            alpha_list.append(alpha)
            if list_x:
                x_list.append(x)
            print(f'iter: {k}\t\tres: {res}')
            k += 1
        if k == self.maxit:
            x = None
        return [x, res_list, alpha_list, x_list]


if __name__ == '__main__':
    pass
