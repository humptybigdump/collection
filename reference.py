import numpy as np
from matplotlib import pyplot as plt
import matplotlib.animation as animation
import pprint


def calc_q(polarisation: str, epsilon: float) -> float:
    """Polarization selective prefactor"""
    if polarisation == "TM":
        return 1 / epsilon
    return 1


def calc_kz(wavelength: float, n_in: float, angle_inc: float) -> float:
    """Transverse component of the wavevector"""
    return 2 * np.pi / wavelength * n_in * np.sin(angle_inc)


def calc_kx(wavelength: float, epsilon: float, kz: float) -> float:
    """Wavevector component normal to the layers"""
    return np.sqrt((2 * np.pi / wavelength) ** 2 * epsilon - kz**2)


def single_layer(
    t: float,
    epsilon: float,
    polarisation: str,
    wavelength: float | np.ndarray,
    kz: float,
):
    """Transfer Matrix of a single layer.
    If wavelength vectorial: first dimension of the output -> wl axis"""
    is_scalar = not np.ndim(wavelength)
    kx = calc_kx(wavelength, epsilon, kz)
    q = calc_q(polarisation, epsilon)
    phi = kx * t
    diag = np.cos(phi)
    off_diag = np.sin(phi)
    M = np.array([[diag, off_diag / (kx * q)], [-off_diag * kx * q, diag]])
    if is_scalar:
        return M
    return np.moveaxis(M, -1, 0)


def transfermatrix(thickness, epsilon, polarisation, wavelength, kz):
    """Computes the transfer matrix for a given stratified medium.
    Parameters
    ----------
    thickness : 1d-array
      Thicknesses of the layers in μm.
    epsilon : 1d-array
      Relative dielectric permittivity of the layers.
    polarisation : str
      Polarisation of the computed field, either 'TE' or 'TM'.
    wavelength : float
      The wavelength of the incident light in μm.
    kz : float
      Transverse wavevector in 1/μm.

    Returns
    -------
    M : 2d-array
    The transfer matrix of the medium.
    """

    M = np.eye(2)
    for t, eps in zip(thickness, epsilon):
        mi = single_layer(t, eps, polarisation, wavelength, kz)
        M = mi @ M
    return M


def spectrum(thickness, epsilon, polarisation, wavelength, angle_inc, n_in, n_out):
    """Computes the reflection and transmission of a stratified medium.
    Parameters
    ----------
    thickness : 1d-array
      Thicknesses of the layers in μm.
    epsilon : 1d-array
      Relative dielectric permittivity of the layers.
    polarisation : str
      Polarisation of the computed field, either 'TE' or 'TM'.
    wavelength : 1d-array
      The wavelength of the incident light in μm.
    angle_inc : float
      The angle of incidence in degree (not radian!).
    n_in, n_out : float
      The refractive indices of the input and output layers.

    Returns
    -------
    t : 1d-array
      Transmitted amplitude
    r : 1d-array
      Reflected amplitude
    T : 1d-array
      Transmitted energy
    R : 1d-array
      Reflected energy
    """
    kz = calc_kz(wavelength, n_in, angle_inc)
    M = transfermatrix(thickness, epsilon, polarisation, wavelength, kz)

    [M11, M12], [M21, M22] = np.moveaxis(M, 0, -1)

    q_in = calc_q(polarisation, n_in**2)
    q_out = calc_q(polarisation, n_out**2)
    kx_in = calc_kx(wavelength, n_in**2, kz)
    kx_out = calc_kx(wavelength, n_out**2, kz)
    qk_in = q_in * kx_in
    qk_out = q_out * kx_out

    denominator = qk_in * M22 + qk_out * M11 + 1j * (M21 - qk_in * qk_out * M12)

    r = (qk_in * M22 - qk_out * M11 - 1j * (M21 + qk_in * qk_out * M12)) / denominator
    t = 2 * qk_in / denominator

    R = np.abs(r) ** 2
    T = np.abs(t) ** 2 * q_out / q_in * np.real(kx_out / kx_in)

    return t, r, T, R


if __name__ == "__main__":
    # Example Values
    eps1 = 2.25
    eps2 = 15.21
    d1 = 0.13  # [μm]
    d2 = 0.05  # [μm]
    N = 5
    polarisation = "TE"
    angle_inc = 0.0
    n_in = 1.0
    n_out = 1.5

    epsilon = np.zeros(2 * N)
    epsilon[0::2] = eps1
    epsilon[1::2] = eps2
    thickness = np.zeros(2 * N)
    thickness[0::2] = d1
    thickness[1::2] = d2

    ## TASK 1
    wavelength = 0.78
    kz = calc_kz(wavelength, n_in, angle_inc)
    M = transfermatrix(thickness, epsilon, polarisation, wavelength, kz)
    print("################## Transfer Matrix ################")
    pprint.pp(M)

    ## TASK 2
    wavelength = np.linspace(0.5, 1.5, 400)
    t, r, T, R = spectrum(
        thickness, epsilon, polarisation, wavelength, angle_inc, n_in, n_out
    )

    plt.figure()
    plt.plot(wavelength, T, label="T")
    plt.plot(wavelength, R, label="R")
    plt.xlabel("wavelength $\lambda$ [um]")
    plt.legend()
    # plt.show()


def field(
    thickness, epsilon, polarisation, wavelength, kz, n_in, n_out, Nx, l_in, l_out
):
    """Computes the field inside a stratified medium.
    The medium starts at x = 0 on the entrance side. The transmitted field
    has a magnitude of unity.

    Parameters
    ----------
    thickness : 1d-array
      Thicknesses of the layers in μm.
    epsilon : 1d-array
      Relative dielectric permittivity of the layers.
    polarisation : str
      Polarisation of the computed field, either 'TE' or 'TM'.
    wavelength : float
      The wavelength of the incident light in μm.
    kz : float
      Transverse wavevector in 1/μm.
    n_in, n_out : float
      The refractive indices of the input and output layers.
    Nx : int
      Number of points where the field will be computed.
    l_in, l_out : float
      Additional thickness of the input and output layers where the field will
      be computed.

    Returns
    -------
    f : 1d-array
      Field structure
    index : 1d-array
      Refractive index distribution
    x : 1d-array
      Spatial coordinates
    """

    kz = calc_kz(wavelength, n_in, angle_inc)

    kx_out = calc_kx(wavelength, n_out**2, kz)
    q_out = calc_q(polarisation, n_out**2)

    thickness_rev = np.concatenate([[l_out], thickness[::-1], [l_in]])
    epsilon_rev = np.concatenate([[n_out**2], epsilon[::-1], [n_in**2]])

    total_thickness = np.sum(thickness)
    x = np.linspace(-l_out, total_thickness + l_in, Nx)
    f = np.zeros(Nx, dtype=complex)  # array to collect the field in
    index = np.zeros(Nx)

    # Field at the interface (column vector)
    field_at_if = np.array([1, -1j * kx_out * q_out])
    field_at_if = field_at_if[:, np.newaxis]

    x_start = x[0]
    for t, eps in zip(thickness_rev, epsilon_rev):
        x_end = x_start + t
        kx = calc_kx(wavelength, eps, kz)
        q = calc_q(polarisation, n_out**2)

        # Calculate the field values only for the current layer (by masking)
        mask_local = np.logical_and(x >= x_start, x < x_end)
        x_local = x[mask_local] - x_start
        index[mask_local] = np.sqrt(eps)

        # Reconstruct the fields from the [F, G].T matrix
        renorm_G = field_at_if[1] / (1j * q * kx)
        F = field_at_if[0]
        forward = (F + renorm_G) / 2
        backward = (F - renorm_G) / 2

        # Save the local fields
        f[mask_local] = forward * np.exp(1j * kx * x_local) + backward * np.exp(
            -1j * kx * x_local
        )

        # Move to next interface
        m_i = single_layer(t, eps, polarisation, wavelength, kz)
        field_at_if = m_i @ field_at_if  ## Question to Markus: why transposed?
        x_start = x_end

    scale = 1
    scale = np.abs(F) / (np.abs(backward) * F)
    # Field is scaled to the incident field instead of the transmitted field
    # (contrary to ex. slides, but according to ex. examples)

    # Turn the coordinates back around:
    return f[::-1] * scale, index[::-1], total_thickness - x[::-1]


def timeanimation(x, f, index, steps, periods):
    """Animation of a quasi-stationary field.
    Parameters
    ----------
    x : 1d-array
      Spatial coordinates
    f : 1d-array
      Field
    index : 1d-array
      Refractive index
    steps : int
      Total number of time points
    periods : int
      Number of the oscillation periods.
    """

    def fu(t):
        """function update to time `t`"""
        return np.real(f * np.exp(-1j * t))

    fig, ax = plt.subplots()
    ts = np.linspace(0, 2 * np.pi * periods, steps)

    y_ax = [-2, 2]
    X, Y = np.meshgrid(x, y_ax)
    ax.pcolormesh(X, Y, np.array([index] * 2))

    line = ax.plot(x, fu(0), color="white")[0]
    ax.set(ylim=y_ax, xlabel="x [um]", ylabel="Amplitude [inc]")

    def update(frame):
        # for each frame, update the data stored on each artist.
        y = fu(ts[frame])
        line.set_xdata(x)
        line.set_ydata(y)
        return line

    ani = animation.FuncAnimation(fig=fig, func=update, frames=steps, interval=30)
    return ani


if __name__ == "__main__":
    ## TASK 3
    f, index, x = field(
        thickness, epsilon, polarisation, 0.78, kz, n_in, n_out, 500, 1.5, 1.5
    )

    fig, [ax1, ax2] = plt.subplots(2, 1, sharex=True)
    ax1.plot(x, f.real, label="real")
    ax1.plot(x, f.imag, label="imag")
    ax1.legend(loc="lower right")
    ax1.set_ylabel("total field")

    ax2.plot(x, index)
    ax2.set_ylabel("index $n$")
    plt.xlabel("$x$ [um]")
    plt.show()

    ## TASK 4
    # ani = timeanimation(x, f, index, 90, 3)
    # ani.save("animation.mp4")
