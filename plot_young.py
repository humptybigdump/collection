import numpy as np
def plot_young(C,view1=30,view2=30,reso=60):
    
    # plot import
    import matplotlib.pyplot as plt
    from mpl_toolkits.mplot3d import Axes3D

    # dicretize angles 
    theta = np.linspace(0., 2.*np.pi, reso)
    phi   = np.linspace(0., np.pi, reso)

    # make 2d grid from angles
    thetaGrid, phiGrid = np.meshgrid(theta, phi)

    # evaluate Young's modulus on grid
    i = 0
    youngGrid = np.zeros((len(theta),len(phi)))
    for i in range(len(theta)):
        for j in range(len(phi)):
            youngGrid[j,i] = young_angle(C,theta[i],phi[j]) # Returns the angle-dependent Young's modulus

    # get max value
    youngMax = np.amax(youngGrid)
    
    # compute X,Y,Z values to print (see documentation)
    X = youngGrid * np.sin(phiGrid) * np.cos(thetaGrid)
    Y = youngGrid * np.sin(phiGrid) * np.sin(thetaGrid)
    Z = youngGrid * np.cos(phiGrid)

    fig = plt.figure()
    #ax = fig.add_subplot(111, projection='3d')
    ax = fig.gca(projection='3d')

    # label axes
    ax.set_xlabel('E_11')
    ax.set_ylabel('E_22')
    ax.set_zlabel('E_33')

    # Set each axis limits to be equal
    ax.set_xlim([-youngMax,youngMax])
    ax.set_ylim([-youngMax,youngMax]) 
    ax.set_zlim([-youngMax,youngMax]) 
    
    # Equally stretch all axes
    #ax.set_aspect("equal")
    
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