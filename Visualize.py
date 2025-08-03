
import numpy as np
from matplotlib.pyplot import contour, clabel, show, imshow, plot, legend, subplots_adjust
import matplotlib.cm as cm


class Visualize(object):
    """
    A basic visualization class for two-dimensional functions, e.g. the Himmelblau function.
    Feel free to extend this class to suit your needs.
    """

    def __init__(self, value, xrange, yrange, levels=[10, 30, 60, 90, 150]):
        """Initialize the Visualize object. value is the method that the returns the function value at any given point."""

        self.X, self.Y = np.meshgrid(xrange, yrange)
        self.levels = levels
        self.Z = value([self.X, self.Y])

    def addContourPlot(self):
        """Plot the contour plot of the function values (corresponding to self.Z)"""

        imshow(self.Z, interpolation='bilinear', origin='lower',
               cmap=cm.get_cmap('ocean_r'), extent=(np.min(self.X), np.max(self.X), np.min(self.Y), np.max(self.Y)))
        cset = contour(self.X, self.Y, self.Z,
                       levels=self.levels, cmap=cm.get_cmap('gray'),)
        clabel(cset, inline=True, fmt='%0.1f', fontsize=10)

    def addXValues(self, x_list, add_legend=True):
        """Plot additional data points from x_list. Might be handy when tracking the progress of Gradient Descent."""

        # Extract the x and y values from the x_list
        x1, x2 = zip(*x_list)
        plot(x1, x2, marker='x', label=f'iters: {len(x_list)-1}, $x_0$: {x_list[0]}\t $x$: {x_list[-1]}')
        if add_legend:
            legend(title='GradientDescent',loc='upper left', bbox_to_anchor=(1,1))
            subplots_adjust(right=0.5)

    def showAll(self):
        show()
