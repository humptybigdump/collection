import numpy as np
from scipy.special import jv, hankel1 # Bessel and hankel function

def refractive_index_Ag(wavelengths):
    # Johnson & Christy silver parameters taken from refractiveindex.info
    d = np.loadtxt('AgJC.txt', delimiter=',')
    wl = d[:,0]
    n = d[:,1] + 1j*d[:,2]
    ninterp = np.interp(wavelengths, wl, n)
    return ninterp

def cyl_mie_coeffs(k0, m_max, radius, n_sc, n_bg, pol):
    """ Mie Coefficients of an infinite cylinder.
    Arguments
    ---------
        k0: float
            vacuum wavenumber
        m_max: (strictly positive) int
            maximum multipolar order
        radius: float
            radius of the cylindrical scatterer
        n_sc: float
            refractive index of the scatterer
        n_bg: float
            background refractive index
        pol: str
            polarization. either "TE" or "TM"
    
    Returns
    -------
        sca: np.ndarray[float]
            coefficients of the scattered field from -m_max to m_max
            (a_m)
        trans: np.ndarray[float]
            coefficients of the transmitted field (inside the cylinder)
            (b_m)
    """
    pass

def total_field(inc, Nx, max_x, k0, radius, n_sc, n_bg, pol):
    """ Evaluates the total field in and around the cylinder 
    on a `Nx by Nx`cartesian grid.
    Arguments
    ---------
        inc: np.ndarray[float]
            coefficients of the incident field. 
            order of the multipole coefficients' m is assumed to be -m_max ... m_max
        Nx: int
            Number of gridpoints used to discretize space
        max_x: float
            absolute extend of the grid (symmetric around origin)
        k0: float
            vacuum wavenumber
        radius: float
            radius of the cylindrical scatterer
        n_sc: float
            refractive index of the scatterer
        n_bg: float
            background refractive index
        pol: str
            polarization, either "TE" or "TM"
    
    Returns
    -------
        field: `Nx by Nx` np.ndarray[complex]
            the total field
        X, Y:  `Nx by Nx` grid centered around the origin
    """
    pass

def scattering_xsection(k0, m_max, radius, n_sc, n_bg, pol):
    """ Evaluates the scattering cross section.
    Arguments
    ---------
        see `cyl_mie_coeffs`
    
    Returns
    -------
        C_sca: float
            scattering crossection
    """
    pass
