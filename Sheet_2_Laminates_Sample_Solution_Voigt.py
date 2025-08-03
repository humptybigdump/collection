import numpy as np

### normal vector ###

# define normal vector in polar coordinates
def normal(theta,phi):
    return np.array([np.sin(phi)*np.cos(theta),
                     np.sin(phi)*np.sin(theta),
                     np.cos(phi)])

# define normal vector dyadic times 4
def normalDyad4(n):
    N = np.outer(n,n)
    Nvec = np.array([N[0,0],N[1,1],N[2,2],N[1,2],N[0,2],N[0,1]])
    return np.outer(Nvec,Nvec)

sq2=np.sqrt(2)

### compute directional Young's modulus and plot

# define Young's modulus in direction n
def youngDir(C,n):
    S = np.linalg.inv(C)
    Ndyad4 = normalDyad4(n)
    return 1./np.dot(np.ravel(S),np.ravel(Ndyad4))

# define Young's modulus in direction n via angles
def youngAngle(C,theta,phi):
    n = normal(theta,phi)
    S = np.linalg.inv(C)
    Ndyad4 = normalDyad4(n)
    return 1./np.dot(np.ravel(S),np.ravel(Ndyad4))

# use plot_surface -- see documentations:
# https://matplotlib.org/mpl_toolkits/mplot3d/tutorial.html
# https://matplotlib.org/tutorials/introductory/pyplot.html

def plotYoung(C, view1=30, view2=30, reso=60):
    # plot import
    import matplotlib.pyplot as plt
    from mpl_toolkits.mplot3d import Axes3D

    # dicretize angles
    theta = np.linspace(0., 2. * np.pi, reso)
    phi = np.linspace(0., np.pi, reso)

    # make 2d grid from angles
    thetaGrid, phiGrid = np.meshgrid(theta, phi)

    # evaluate Young's modulus on grid
    i = 0
    youngGrid = np.zeros((len(theta), len(phi)))
    for i in range(len(theta)):
        for j in range(len(phi)):
            youngGrid[j, i] = youngAngle(C, theta[i], phi[j])

    # get max value
    youngMax = np.amax(youngGrid)

    # compute X,Y,Z values to print (see documentation)
    X = youngGrid * np.sin(phiGrid) * np.cos(thetaGrid)
    Y = youngGrid * np.sin(phiGrid) * np.sin(thetaGrid)
    Z = youngGrid * np.cos(phiGrid)

    fig = plt.figure()
    # ax = fig.add_subplot(111, projection='3d')
    ax = fig.add_subplot(projection='3d')

    # label axes
    ax.set_xlabel('E_11')
    ax.set_ylabel('E_22')
    ax.set_zlabel('E_33')

    # Set each axis limits to be equal
    ax.set_xlim([-youngMax, youngMax])
    ax.set_ylim([-youngMax, youngMax])
    ax.set_zlim([-youngMax, youngMax])

    # Equally stretch all axes
    # ax.set_aspect("equal")

    # edit view point angles
    ax.view_init(elev=view1, azim=view2)

    # plot
    plot = ax.plot_surface(
        X, Y, Z, rstride=1, cstride=1, cmap=plt.get_cmap('nipy_spectral'),
        linewidth=0, antialiased=False, alpha=0.8)

    # colormaps
    # 'flag', 'prism', 'ocean', 'gist_earth', 'terrain',
    # 'gist_stern', 'gnuplot', 'gnuplot2', 'CMRmap', 'cubehelix',
    # 'brg', 'gist_rainbow', 'rainbow', 'jet', 'nipy_spectral', 'gist_ncar'

    plt.show()

def isoStiff(young,nu):
    stiffness = young/((1+nu)*(1-2*nu))*np.array([[1-nu,nu,nu,0,0,0],
                                             [nu,1-nu,nu,0,0,0],
                                             [nu,nu,1-nu,0,0,0],
                                             [0,0,0,(1-2*nu)/2,0,0],
                                             [0,0,0,0,(1-2*nu)/2,0],
                                             [0,0,0,0,0,(1-2*nu)/2]])
    return stiffness

# define transversely isotropic stiffness
# use compliance in Voigt notation

# x-direction
def transisoXStiff(EL,ET,GLT,nuLT,nuTT):
    S = np.array([[1/EL,-nuLT/EL,-nuLT/EL,0,0,0],
                [-nuLT/EL,1/ET,-nuTT/ET,0,0,0],
                [-nuLT/EL,-nuTT/ET,1/ET,0,0,0],
                [0,0,0,2*(1+nuTT)/ET,0,0],
                [0,0,0,0,1/GLT,0],
                [0,0,0,0,0,1/GLT]])
    return(np.linalg.inv(S))

# y-direction
def transisoYStiff(EL,ET,GLT,nuLT,nuTT):
    S = np.array([[1/ET,-nuLT/EL,-nuTT/ET,0,0,0],
                [-nuLT/EL,1/EL,-nuLT/EL,0,0,0],
                [-nuTT/ET,-nuLT/EL,1/ET,0,0,0],
                [0,0,0,1/GLT,0,0],
                [0,0,0,0,2*(1+nuTT)/ET,0],
                [0,0,0,0,0,1/GLT]])
    return(np.linalg.inv(S))

# z-direction
def transisoZStiff(EL,ET,GLT,nuLT,nuTT):
    S = np.array([[1/ET,-nuTT/ET,-nuLT/EL,0,0,0],
                [-nuTT/ET,1/ET,-nuLT/EL,0,0,0],
                [-nuLT/EL,-nuLT/EL,1/EL,0,0,0],
                [0,0,0,1/GLT,0,0],
                [0,0,0,0,1/GLT,0],
                [0,0,0,0,0,2*(1+nuTT)/ET]])
    return(np.linalg.inv(S))

### arbitrary stiffnesses

# define exemplary stiffness from Puck11
def exempStiff1():
    C = np.array([8.1361e+02,5.0933e+02,5.0933e+02,0.0000e+00,0.0000e+00,0.0000e+00,
                5.0933e+02,4.3105e+03,1.8556e+03,0.0000e+00,0.0000e+00,0.0000e+00,
                5.0933e+02,1.8556e+03,4.3105e+03,0.0000e+00,0.0000e+00,0.0000e+00,
                0.0000e+00,0.0000e+00,0.0000e+00,1.2274e+03,0.0000e+00,0.0000e+00,
                0.0000e+00,0.0000e+00,0.0000e+00,0.0000e+00,1.2274e+03,0.0000e+00,
                0.0000e+00,0.0000e+00,0.0000e+00,0.0000e+00,0.0000e+00,1.2274e+03]).reshape(6,6)
    return(C)

# define exemplary stiffness from Puck all cases combined loading
def exempStiff2():
    C = np.array([6.0276e+02,2.8437e+01,2.8437e+01,0.0000e+00,0.0000e+00,0.0000e+00,
                2.8437e+01,6.7668e+02,-2.5735e+02,0.0000e+00,0.0000e+00,0.0000e+00,
                2.8437e+01,-2.5735e+02,6.7668e+02,0.0000e+00,0.0000e+00,0.0000e+00,
                0.0000e+00,0.0000e+00,0.0000e+00,1.6337e+02,0.0000e+00,0.0000e+00,
                0.0000e+00,0.0000e+00,0.0000e+00,0.0000e+00,2.7450e+02,0.0000e+00,
                -0.0000e+00,-0.0000e+00,-0.0000e+00,-0.0000e+00,-0.0000e+00,2.7450e+02]).reshape(6,6)
    return(C)

# Example 1
# C1 = isoStiff(3400,0.385)/1000
# print(C1)
# plotYoung(C1)

# Example 2
C2 = exempStiff1()/1000
print(C2)
plotYoung(C2)
# # Example 3
# C3 = exempStiff2()/1000
# print(C3)
# plotYoung(C3)
#
# # Example 4 (SMC fiber bundle in given direction)
# CsmcX = transisoXStiff(37.73,10.33,3.64,0.292,0.477)
# print(CsmcX)
# plotYoung(CsmcX)
#
# CsmcY = transisoYStiff(37.73,10.33,3.64,0.292,0.477)
# print(CsmcY)
# plotYoung(CsmcY)
#
# CsmcZ = transisoZStiff(37.73,10.33,3.64,0.292,0.477)
# print(CsmcZ)
# plotYoung(CsmcZ)


def largestEval(C):
    evals,evecs = np.linalg.eig(C)
    print(evals)
    return np.amax(evals)

# define gamma in arbitrary direction (Voigt notation compliance)
def gamma(n):
    gamma = np.zeros((6,6))
    # first row
    gamma[0,0] =   2*n[0]*n[0] - n[0]*n[0]*n[0]*n[0]
    gamma[0,1] =               - n[0]*n[0]*n[1]*n[1]
    gamma[0,2] =               - n[0]*n[0]*n[2]*n[2]
    gamma[0,3] = (             - n[0]*n[0]*n[1]*n[2] )*2
    gamma[0,4] = (   n[0]*n[2] - n[0]*n[0]*n[0]*n[2] )*2
    gamma[0,5] = (   n[0]*n[1] - n[0]*n[0]*n[0]*n[1] )*2
    # second row
    gamma[1,0] =   gamma[0,1]
    gamma[1,1] =   2*n[1]*n[1] - n[1]*n[1]*n[1]*n[1]
    gamma[1,2] =               - n[1]*n[1]*n[2]*n[2]
    gamma[1,3] = (   n[1]*n[2] - n[1]*n[1]*n[1]*n[2] )*2
    gamma[1,4] = (             - n[1]*n[1]*n[0]*n[2] )*2
    gamma[1,5] = (   n[0]*n[1] - n[1]*n[1]*n[0]*n[1] )*2
    # third row
    gamma[2,0] =   gamma[0,2]
    gamma[2,1] =   gamma[1,2]
    gamma[2,2] =   2*n[2]*n[2] - n[2]*n[2]*n[2]*n[2]
    gamma[2,3] = (   n[1]*n[2] - n[2]*n[2]*n[1]*n[2] )*2
    gamma[2,4] = (   n[0]*n[2] - n[2]*n[2]*n[0]*n[2] )*2
    gamma[2,5] = (             - n[2]*n[2]*n[0]*n[1] )*2
    # fourth row
    gamma[3,0] =   gamma[0,3]
    gamma[3,1] =   gamma[1,3]
    gamma[3,2] =   gamma[2,3]
    gamma[3,3] = ( 0.5*(n[1]*n[1]+n[2]*n[2]) - n[1]*n[2]*n[1]*n[2] )*4
    gamma[3,4] = ( 0.5*(n[0]*n[1])           - n[1]*n[2]*n[0]*n[2] )*4
    gamma[3,5] = ( 0.5*(n[0]*n[2])           - n[1]*n[2]*n[0]*n[1] )*4
    # fifth row
    gamma[4,0] =   gamma[0,4]
    gamma[4,1] =   gamma[1,4]
    gamma[4,2] =   gamma[2,4]
    gamma[4,3] =   gamma[3,4]
    gamma[4,4] = ( 0.5*(n[0]*n[0]+n[2]*n[2]) - n[0]*n[2]*n[0]*n[2] )*4
    gamma[4,5] = ( 0.5*(n[1]*n[2])           - n[0]*n[2]*n[0]*n[1] )*4
    # sixth row
    gamma[5,0] =   gamma[0,5]
    gamma[5,1] =   gamma[1,5]
    gamma[5,2] =   gamma[2,5]
    gamma[5,3] =   gamma[3,5]
    gamma[5,4] =   gamma[4,5]
    gamma[5,5] = ( 0.5*(n[0]*n[0]+n[1]*n[1]) - n[0]*n[1]*n[0]*n[1] )*4
    # return
    return gamma
n = np.array([1/np.sqrt(3),1/np.sqrt(3),1/np.sqrt(3)])
print(gamma(n))
#identity matrix 6x6 (Voigt notation stiffness)
id4 = np.diag([1,1,1,0.5,0.5,0.5])

# rhs for one stiffness according to milton formula for laminates
def singlerhs(C,alpha,n):
    term1 = np.linalg.inv(alpha*id4-C)
    rhs = np.linalg.inv(alpha*term1-gamma(n))
    return rhs

### 2 phases

# compute average of milton rhs for 2 phases
# ATTENTION: v1+v2 = 1
def average2(C1,C2,v1,v2,n):
    alpha1 = np.trace(C1)
    alpha2 = np.trace(C2)
    alpha = np.amax([alpha1,alpha2])
    return alpha,v1*singlerhs(C1,alpha,n)+v2*singlerhs(C2,alpha,n)

# compute effective laminate stiffness for 2 phases
def effective2(C1,C2,v1,v2,n):
    alpha,rhs = average2(C1,C2,v1,v2,n)
    term1 = (np.linalg.inv(rhs)+gamma(n))/alpha
    Ceff = alpha*id4-np.linalg.inv(term1)
    return Ceff

### N phases

# get maximum alpha of all alphas of all stiffnesses
def getalpha(C):
    alphalist = np.zeros(len(C))
    for i in range(len(C)):
        alphalist[i] = np.trace(C[i])
    alpha = np.amax(alphalist)
    return alpha

# compute average of milton rhs for N phases
# stiffnesses and volume fractions are given as lists
def average(C,vol,n):
    assert len(C)==len(vol), 'stiffness and volume fractions must match'
    assert abs(np.sum(vol)-1.)<10e-6, 'volume fractions must sum to 1'
    alpha = getalpha(C)
    result = np.zeros((6,6))
    for i in range(len(C)):
        result += vol[i]*singlerhs(C[i],alpha,n)
    return result

# compute effective laminate stiffness for N phases
def effective(C,vol,n):
    assert len(C)==len(vol), 'stiffness and volume fractions must match'
    assert abs(np.sum(vol)-1.)<10e-6, 'volume fractions must sum to 1'
    alpha = getalpha(C)
    rhs = average(C,vol,n)
    term1 = (np.linalg.inv(rhs)+gamma(n))/alpha
    Ceff = alpha*id4-np.linalg.inv(term1)
    return Ceff

### test 2 phases

e1 = np.array([1,0,0])
e2 = np.array([0,1,0])
e3 = np.array([0,0,1])
n1 = np.array([1,1,1])/np.sqrt(3)
n2 = np.array([1,1,0])/np.sqrt(2)
n3 = np.array([1,2,3])/np.sqrt(14)
C1 = isoStiff(3400,0.385)/1000 # UPPH
C5 = isoStiff(72000,0.22)/1000 # E-glass
v1 = 0.5
v4 = 0.5
Ceff = effective2(C1,C5,v1,v4,e3)
print(Ceff)
plotYoung(Ceff)

### test N phases

# e1 = np.array([1,0,0])
# e2 = np.array([0,1,0])
# e3 = np.array([0,0,1])
# n1 = np.array([1,1,1])/np.sqrt(3)
# n2 = np.array([1,1,0])/np.sqrt(2)
# n3 = np.array([1,2,3])/np.sqrt(14)
# C1 = isoStiff(3400,0.385)/1000 # UPPH
# C2 = exempStiff1()/1000 # damaged UPPH in 11-direction
# C3 = exempStiff2()/1000 # damaged UPPH in all direction
# C4 = transisoXStiff(37.73,10.33,3.64,0.292,0.477) # SMC bundle in x
# C5 = isoStiff(72000,0.2)/1000 # E-glass
# C6 = transisoYStiff(37.73,10.33,3.64,0.292,0.477)
# C7 = transisoZStiff(37.73,10.33,3.64,0.292,0.477)
# v1 = 0.0
# v2 = 0.0
# v3 = 0.0
# v4 = 0.5
# v5 = 0.5
# v6 = 0.0
# v7 = 0.0
# C = [C1,C2,C3,C4,C5,C6,C7]
# vol = [v1,v2,v3,v4,v5,v6,v7]
# Ceff = effective(C,vol,e2)
# print(Ceff)
# plotYoung(Ceff)