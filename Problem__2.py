import numpy as np

###
### This is just the same Problem class we have been using in the previous excercises. 
### Obviously, you can use your own if you like.
###

class Problem:
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
    def gradient(self,x):
        raise Exception(
            "This class doesn't seem to implement the value method")
    def hessian(self,x):
        raise Exception(
            "This class doesn't seem to implement the value method")

class f1(Problem):
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