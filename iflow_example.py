""" Integrate test functions of the i-flow paper [1]

    [1] i-flow: High-dimensional Integration
        and Sampling with Normalizing Flows
    by: Christina Gao, Joshua Isaacson, and Claudius Krause
    arXiv: 2001.05486
"""

import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import tensorflow_probability as tfp
from scipy.special import erf
import vegas, gvar

from absl import app, flags

from iflow.integration import integrator
from iflow.integration import couplings

tfd = tfp.distributions  # pylint: disable=invalid-name
tfb = tfp.bijectors  # pylint: disable=invalid-name
tf.keras.backend.set_floatx('float64')

FLAGS = flags.FLAGS
flags.DEFINE_integer('epochs', 1000, 'Number of epochs to train',
                     short_name='e')
flags.DEFINE_integer('ptspepoch', 1000, 'Number of points to sample per epoch',
                     short_name='p')
flags.DEFINE_float('precision', 1e-3, 'Target precision in integrator comparison',
                   short_name='tp')

class TestFunctions:
    def __init__(self):
        self.calls = 0

    def f(self, x):
        self.calls += 1
        # Rescale random variable from [0,1] to [-5,5]
        y = x*10-5
        return tf.exp(-(y[..., 1]- tf.cos(y[..., 0]))**2/2.-y[..., 0]**2/2.)/(2.*np.pi)*100.

    def f_np(self, x):
        self.calls += 1
        # Rescale random variable from [0,1] to [-5,5]
        y = x*10-5
        return np.exp(-(y[..., 1]- np.cos(y[..., 0]))**2/2.-y[..., 0]**2/2.)/(2.*np.pi)*100.


def build(in_features, out_features, options):
    """ Builds a dense NN.

    The output layer is initialized to 0, so the first pass
    before training gives the identity transformation.

    Arguments:
        in_features (int): dimensionality of the inputs space
        out_features (int): dimensionality of the output space
        options: additional arguments, not used at the moment

    Returns:
        A tf.keras.models.Model instance

    """
    del options

    invals = tf.keras.layers.Input(in_features, dtype=tf.float64)
    hidden = tf.keras.layers.Dense(32, activation='relu')(invals)
    hidden = tf.keras.layers.Dense(32, activation='relu')(hidden)
    hidden = tf.keras.layers.Dense(32, activation='relu')(hidden)
    hidden = tf.keras.layers.Dense(32, activation='relu')(hidden)
    outputs = tf.keras.layers.Dense(out_features, bias_initializer='zeros',
                                    kernel_initializer='zeros')(hidden)
    model = tf.keras.models.Model(invals, outputs)
    model.summary()
    return model


def mask_flip(mask):
    """ Interchange 0 <-> 1 in the mask. """
    return 1-mask


def binary_list(inval, length):
    """ Convert x into a binary list of length l. """
    return np.array([int(i) for i in np.binary_repr(inval, length)])


def binary_masks(ndims):
    """ Create binary masks for to account for symmetries. """
    n_masks = int(np.ceil(np.log2(ndims)))
    sub_masks = np.transpose(np.array(
        [binary_list(i, n_masks)
         for i in range(ndims)]))[::-1]
    flip_masks = mask_flip(sub_masks)

    # Combine masks
    masks = np.empty((2*n_masks, ndims))
    masks[0::2] = flip_masks
    masks[1::2] = sub_masks

    return masks


def build_iflow(func, ndims):
    """ Build the iflow integrator

    Args:
        func: integrand
        ndims (int): dimensionality of the integrand

    Returns: Integrator: iflow Integrator object

    """
    masks = binary_masks(ndims)
    bijector = []
    for mask in masks:
        bijector.append(couplings.PiecewiseRationalQuadratic(mask, build,
                                                             num_bins=16,
                                                             blob=None,
                                                             options=None))
    bijector = tfb.Chain(list(reversed(bijector)))
    low = np.zeros(ndims, dtype=np.float64)
    high = np.ones(ndims, dtype=np.float64)
    dist = tfd.Uniform(low=low, high=high)
    dist = tfd.Independent(distribution=dist,
                           reinterpreted_batch_ndims=1)
    dist = tfd.TransformedDistribution(
        distribution=dist,
        bijector=bijector)

    optimizer = tf.keras.optimizers.Adam(1e-3, clipnorm=10.0)
    integrate = integrator.Integrator(func, dist, optimizer,
                                      loss_func='exponential')

    return integrate


def train_iflow(integrate, ptspepoch, epochs):
    """ Run the iflow integrator

    Args:
        integrate (Integrator): iflow Integrator class object
        ptspepoch (int): number of points per epoch in training
        epochs (int): number of epochs for training

    Returns:
        numpy.ndarray(float): value of loss (mean) and its uncertainty (standard deviation)

    """
    means = np.zeros(epochs)
    stddevs = np.zeros(epochs)
    for epoch in range(epochs):
        loss, integral, error = integrate.train_one_step(ptspepoch,
                                                         integral=True)
        means[epoch] = integral
        stddevs[epoch] = error
        _, current_precision = variance_weighted_result(means[:epoch+1], stddevs[:epoch+1])
        if epoch % 10 == 0:
            print('Epoch: {:3d} Loss = {:8e} Integral = '
                  '{:8e} +/- {:8e} Total uncertainty = {:8e}'.format(epoch, loss,
                                                                     integral, error,
                                                                     current_precision))

    return means, stddevs

def train_iflow_target(integrate, ptspepoch, target):
    """ Run the iflow integrator

    Args:
        integrate (Integrator): iflow Integrator class object
        ptspepoch (int): number of points per epoch in training
        target (float): target precision of final integral

    Returns:
        numpy.ndarray(float): integral estimations and its uncertainty of each epoch

    """
    means = []
    stddevs = []
    current_precision = 1e99
    epoch = 0
    while current_precision > target:
        loss, integral, error = integrate.train_one_step(ptspepoch,
                                                         integral=True)
        means.append(integral)
        stddevs.append(error)
        _, current_precision = variance_weighted_result(np.array(means), np.array(stddevs))
        if epoch % 10 == 0:
            print('Epoch: {:3d} Loss = {:8e} Integral = '
                  '{:8e} +/- {:8e} Total uncertainty = {:8e}'.format(epoch, loss,
                                                                     integral, error,
                                                                     current_precision))
        epoch += 1
    return np.array(means), np.array(stddevs)

def sample_iflow(integrate, ptspepoch, epochs):
    """ Sample from the iflow integrator

    Args:
        integrate (Integrator): iflow Integrator class object
        ptspepoch (int): number of points per epoch in training
        epochs (int): number of epochs for training

    Returns:
        (tuple): mean and stddev numpy arrays

    """
    # defining a reduced number of epochs for integral evaluation
    red_epochs = int(epochs/5)

    # mean and stddev of trained NF
    print('Estimating integral from trained network')
    means_t = []
    stddevs_t = []
    for _ in range(red_epochs+1):
        mean, var = integrate.integrate(ptspepoch)
        means_t.append(mean)
        stddevs_t.append(tf.sqrt(var/(ptspepoch-1.)).numpy())
    return np.array(means_t), np.array(stddevs_t)

def rel_unc(mean_a, unc_a, mean_b, unc_b):
    """  Relative uncertainty, for Table III """
    ret = mean_a - mean_b
    sqr = np.sqrt(unc_a**2 + unc_b**2)
    ret = ret/sqr
    return ret

def variance_weighted_result(means, stddevs):
    """ Computes weighted mean and stddev of given means and
        stddevs arrays, using Inverse-variance weighting
    """
    assert np.size(means) == np.size(stddevs)
    assert means.shape == stddevs.shape
    variance = 1./np.sum(1./stddevs**2, axis=-1)
    mean = np.sum(means/(stddevs**2), axis=-1)
    mean *= variance
    return mean, np.sqrt(variance)

def variance_unweight(means_wgt, stddevs_wgt):
    """ Computes mean and stddev of individual run given
        means and stddevs arrays that are weighted with
        Inverse-variance weighting up to that run
    """
    inv_var = 1./stddevs_wgt[1:]**2 - 1./stddevs_wgt[:-1]**2
    stddevs = 1./np.sqrt(inv_var)
    stddevs = np.insert(stddevs, 0, stddevs_wgt[0])
    means = means_wgt[1:]/(stddevs_wgt[1:]**2) - means_wgt[:-1]/(stddevs_wgt[:-1]**2)
    means *= stddevs[1:]**2
    means = np.insert(means, 0, means_wgt[0])
    return means, stddevs

def main(argv):
    """ Main function for test runs. """

    func = TestFunctions()
    ndims = 2

    integrand = func.f
    integrand_np = func.f_np
    target = 1.

    print("Target value of the Integral in {:d} dimensions is {:.6e}".format(
        ndims, target))

    npts = 1000
    pts = np.array(np.random.rand(npts, ndims), dtype=np.float64)
    value = integrand(pts)

    format_string = ('Crude MC of test function based on '
                     '{:d} points: {:.3e} +/- {:.3e}')
    print(format_string.format(npts, np.mean(value),
                               np.std(value)/np.sqrt(npts)))

    epochs = FLAGS.epochs
    ptspepoch = FLAGS.ptspepoch
    target_precision = FLAGS.precision * target

    if True:
        print("In target mode with absolute precision {}, based on relative precision {}".format(
            target_precision, FLAGS.precision))
        integrate = build_iflow(integrand, ndims)
        mean_t, err_t = train_iflow_target(integrate, ptspepoch, target_precision)
        num_epochs = len(mean_t)
        x_values = np.arange(ptspepoch, (num_epochs+1) * ptspepoch, ptspepoch)
        iflow_mean_wgt, iflow_err_wgt = variance_weighted_result(mean_t, err_t)

        print("Results for {:d} dimensions:".format(ndims))
        print("Weighted iflow result is {:.5e} +/- {:.5e}.".format(
            iflow_mean_wgt, iflow_err_wgt))
        print("Relative Uncertainty iflow result is {:.3f}".format(
            rel_unc(iflow_mean_wgt, iflow_err_wgt, target, 0.)))
        print("i-flow needed {:d} epochs and {:d} function calls".format(num_epochs,
                                                                         num_epochs*ptspepoch))


if __name__ == '__main__':
    app.run(main)
