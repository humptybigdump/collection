# -*- coding: utf-8 -*-
def geopowerf(Qvec, Tvec, ND):
    """
    Created on Sat Nov  2 16:39:21 2024
    
    @author: Emmanuel
    
    Generate 2D and 3D figures showing in colour the geothermal power output
    from given production flow rate and temperature ranges.
    Contours are also shown on the plot.
    A figure is automatically saved with a hard-coded name
     
    INPUT:
     Qvec: shape (1,n) row vector of the production flow rate range in l/s.
           Ex.: np.array([(np.arange(0,151,10, dtype=float))])
     Tvec: shape (1,m) row vector of the production temperature in degC.
           Ex.: np.array([(np.arange(70,200,10, dtype=float))])
     ND: either 2 for 2D plot or 3 for 3D plot
    
    OUTPUT:
      GP: shape(n,m) matrix of the geothermal power for all flow rate-
      temperature combinations
    """    
    #============================================================================
    # Import libraries
    #============================================================================
    import numpy as np
    import matplotlib.pyplot as plt
    # to play with the colormaps
    from matplotlib import cm
    
    #============================================================================
    # Create variables
    #============================================================================
    
    rho = 1000 # kg/m3
    cp = 4200  # J/kg/K
    tinj = 70  # degC
    
    # below: not commented if this code is a script
    # Qvec = np.array([(np.arange(0,151,1, dtype=float))]) # L/s => WARNING, this is not SI units!
    # Tvec = np.array([(np.arange(70,200,1, dtype=float))]) # degC
      
    #============================================================================
    # Calculate the geothermal power
    #============================================================================
    
    GP = rho*cp*1e-3*np.matmul(Qvec.transpose(),(Tvec-tinj)); # only valid if Qvec and Tvec are row vectors
    
    #============================================================================
    # Show result
    #============================================================================
    
    # use pcolormesh
    # to control x-axis and y-axis values, one need to create meshgrid corresponding 
    # to Tvec and Qvec
    TT,QQ = np.meshgrid(Tvec, Qvec)
    
    # ND = 2 # variable to tell whether we want a 2D or 3D plot
    
    if ND==2:
        # for a 2D representation
        fig, ax=plt.subplots(1,1,
                            layout="constrained")
        
        # use pcolormesh
        pcol=ax.pcolormesh(TT, QQ, GP*1e-6, cmap=cm.jet) # Gpower is plotted in MW
        # pcol=ax.pcolormesh(TT, QQ, GP*1e-6, cmap=cm.jet.resampled(8)) # Only 8 colors in the colormap
        # pcol=ax.pcolormesh(TT, QQ, GP*1e-6, shading="gouraud", cmap=cm.jet.resampled(8)) # Interpolate between values
    
        # add the colorbar
        cbar = plt.colorbar(pcol)
        cbar.ax.set_ylabel("Geothermal power [MW]")
    elif ND==3:
        # for a 3D presentation
        fig, ax=plt.subplots(1,1,
                             layout="constrained",
                             subplot_kw={"projection": "3d"})
        
        # use plot_surface
        pcol=ax.plot_surface(TT, QQ, GP*1e-6, cmap=cm.jet) # Gpower is plotted in MW
        ax.set_zlabel("Geothermal power [MW]")
        # add the colorbar
        cbar = plt.colorbar(pcol, location='bottom', shrink=0.6)
        cbar.ax.set_xlabel("Geothermal power [MW]")
          
    # in both dimensions show the following    
    ax.set_xlabel("Production temperature (°C)")
    ax.set_ylabel("Production rate (l/s)")
    
    # now add the contours
    ct=ax.contour(TT, QQ, GP*1e-6, levels=np.linspace(0, np.max(np.max(GP))*1e-6, 10), colors="black");
    ax.clabel(ct, inline=True, fontsize=10)
    
    # display figure
    plt.show()    
    # save figure
    if ND==2:
        plt.savefig('geopower_2d.png', dpi=200, format='png') # save file
    elif ND==3:
        plt.savefig('geopower_3d.png', dpi=200, format='png') # save file
          
    # close figure
    plt.close()
    
    # return GP
    return GP