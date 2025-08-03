import numpy as np
import treams

### Create a spherical wave
swb = treams.SphericalWaveBasis.default(lmax)
wave = treams.spherical_wave(l, # called "n" in lecture slides,
                             m,
                             1, # 1 for electric, 0 for magnetic multipoles,
                             k0=k0, # wave number in vacuum,
                             basis=swb,
                             material=epsilon_of_ambient_medium,
                             modetype='singular', # outgoing fields,
                             poltype='parity' # electric & magnetic multipoles
                             )

### Evaluate the electric field of a wave on a grid
# grid should be a 4-dimensional array with dimensions (nx,ny,nz,3), containing
# the X, Y, and Z grids (each with dims (nx,ny,nz)) from 'meshgrid' stacked
# together.
E = wave.efield(grid)
# "E" is now a (nx,ny,nz,3) array where the last dimension gives the xyz components.
# If you only need a single point, this works:
E_singlepoint = wave.efield(np.array([x,y,z]))
# For further manipulation / plotting, you may want to convert from treams'
# "physics arrays" to plain numpy arrays to avoid useless warnings
E = np.array(E).squeeze()

### Get the T-matrix of a sphere
T = treams.TMatrix.sphere(lmax,
                          k0,
                          [radius],
                          [epsilon_of_sphere, epsilon_of_ambient_medium]
                          ).changepoltype()  # ensures we're in the electric/magnetic basis

### Couple multiple objects together
# positions should be an array of shape (number_of_objects, 3) holding the xyz
# coordinates of each object.
Tc = treams.TMatrix.cluster(list_of_T_matrices, positions)
Tc = Tc.interaction.solve()  # calculates the interaction and stores it in the cluster T-matrix

### Get the scattered field (in terms of multipole fields)
waves_scattered = T_matrix_of_cluster @ incident_wave.expand(T_matrix_of_cluster.basis, 'regular')

### Calculate the scattering and extinction cross sections
xs_scattering, xs_extinction = T_matrix.xs(incident_wave)



### Create a plane wave for single-particle/cluster scattering calculations
planewave = treams.plane_wave([1,0,0], # propagation direction [x,y,z]
                              1, # 1 or 0, switches between polarizations
                              k0=k0,
                              material=epsilon_of_ambient_medium,
                              poltype='parity')

### Create a plane wave basis (corresponding to a square lattice and parallel wave vector)
lattice = treams.Lattice.square(a) # a is the lattice constant
pwb = treams.PlaneWaveBasisByComp.diffr_orders(
    kpar,           # component of the wavevector parallel to the lattice (in our case [kx, ky]])
    lattice,    
    b*2*np.pi/a     # Maximum momentum transfer (from kpar) see Hint in excercise slides
)

### Solve for the renormalized T-matrix of a scatterer on an infinite lattice
# (uses exponentially converging Ewald summation to evaluate lattice sum)
spheres = sphere.latticeinteraction.solve(lattice, kpar)

### Construct SMatrices from renormalized T-matrix
# Note that the 'array' given by this function represents the array as an
# infinitely thin object at z = 0. Therefore, you must explicitly add the space
# in z-direction taken up by the spheres. This is done by stacking (see below)
# this S-matrix with 'propagation' S-matrices (see below):
#   propagation in embedding medium, distance = radius of the sphere
#   array S-matrix
#   propagation in embedding medium, distance = radius of the sphere
array = treams.SMatrices.from_array(T_matrix_of_sphere, planewave_basis)

### SMatrices of a dielectric interface 1->2 (the forward propagation for 
# SMatrices is in positive z-direction (up))
inter = treams.SMatrices.interface(planewave_basis, k0, [eps1, eps2])

### SMatrices of a propagation throug dielectric material
prop = treams.SMatrices.propagation(propagation_vector, # [x, y, z] of propagation
                                    planewave_basis, k0, 
                                    material #given a treams.Material
                                    )

# To stack a list of SMatrices on top of each other use:
smat = treams.SMatrices.stack([SMatrices, SMatrices, ...]).changepoltype("parity")

### Create a plane wave for periodic-array scattering calculations
planewave = treams.plane_wave([1,0,0], # xy wave vector components [x,y]
                              1, # 1 or 0, switches between polarizations
                              k0=k0,
                              basis=planewave_basis,
                              material=epsilon_of_ambient_medium,
                              poltype='parity')

# To calculate transmitted and reflected fields in the planewave basis of
# the S-matrix use
trans, refl = smat.illuminate(planewave)
# Get the zeroth diffraction order amplitude for the 1 and 0 polarizations:
zeroth_pol1 = trans[0]
zeroth_pol2 = trans[1]

# Calculate the total power transmitted and reflected (all diffr. orders)
T, R = smat.tr(planewave)
