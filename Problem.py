import numpy as np


class AbstractFunction:
    def __init__(self) -> None:
        pass

    def t_n(self, n):
        twos = np.ones(n)*2
        ones = np.ones(n-1)
        Q = np.diag(twos)-np.diag(ones, -1)-np.diag(ones, 1)
        return Q

    def value(self, x):
        raise Exception(
            "This class doesn't seem to implement the value method")

    def gradient(self, x):
        raise Exception(
            "This class doesn't seem to implement the value method")

    def hessian(self, x):
        raise Exception(
            "This class doesn't seem to implement the value method")

    @property
    def has_L(self):
        return hasattr(self, 'L')

    @property
    def has_mu(self):
        return hasattr(self, 'mu')

    @property
    def has_Q(self):
        return hasattr(self, 'Q')


class f1(AbstractFunction):
    def __init__(self) -> None:
        """
        Himmelblau function
        """
        super().__init__()

    def value(self, x):
        return (x[0]**2+x[1]-11)**2+(x[0]+x[1]**2-7)**2

    def gradient(self, x):
        return np.array([4*x[0]*(x[0]**2+x[1]-11)+(x[0]+x[1]**2-7),
                         (x[0]**2+x[1]-11)+4*x[1] * (x[0]+x[1]**2-7)])

    def hessian(self, x):
        return np.array([[12*x[0]**2+4*x[1]-43, 4*x[0]+2*x[1]],
                         [2*x[0]+4*x[1], 4*x[0]+12*x[1]**2-27]])


class f2(AbstractFunction):
    def __init__(self, n) -> None:
        """
        Quadratic function
        """
        super().__init__()

        self.p = np.ones(n)
        self.Q = self.t_n(n)

        evals = np.linalg.eigvals(self.Q)
        self.L = np.max(evals)
        self.mu = np.min(evals)

    def value(self, x):
        return 0.5*np.dot(x, self.Q@x)+np.dot(self.p, x)

    def gradient(self, x):
        return self.Q@x+self.p

    def hessian(self, x):
        return self.Q
    


class f3(AbstractFunction):
    def __init__(self, n) -> None:
        super().__init__()
        self.Q = self.t_n(n)
        evals = np.linalg.eigvals(self.Q)
        self.L = np.max(evals)

    def value(self, x):
        self.sum = 1+np.dot(x, self.Q@x)
        return np.sqrt(1+np.dot(x, self.Q@x))

    def gradient(self, x):
        return self.Q@x/self.value(x)

    def hessian(self, x):
        return self.Q/self.value(x)-self.sum**(-3/2)*np.outer(self.Q@x, self.Q@x)


class f4(f3):
    def __init__(self, n) -> None:
        super().__init__(n)
        evals = np.linalg.eigvals(self.Q)
        self.L = 2*np.max(evals)
        self.mu = np.min(evals)
    def value(self, x):
        return super().value(x)+0.5*np.dot(x, self.Q@x)

    def gradient(self, x):
        return super().gradient(x)+self.Q@x

    def hessian(self, x):
        return super().hessian(x)+self.Q

class f5(AbstractFunction):
    def __init__(self,n) -> None:
        super().__init__(n)
        p=20
        a_list = np.random.random([p,n])
        b_list = np.random.random(p)

    def value(self,x):
        return np.log()


if __name__ == '__main__':
    myf1 = f1()
    myf2 = f2(5)
    myf3 = f3(5)
    myf4 = f4(5)

    hessian = myf1.hessian(np.ones(5))
    evs = np.linalg.eigvals(hessian)
    print('Done')
