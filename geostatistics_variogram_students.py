import numpy as np
import scipy as sp
import matplotlib.pyplot as plt

# Computes the euclidean distance between two points
def distance(point_1, point_2):

    return np.sqrt((point_1[0] - point_2[0])**2 + (point_1[1] - point_2[1])**2)

# Returns True if two points are aligned along the given azimuth (with a tolerance)
def test_alignment(point_1, point_2, azim):

    dist = distance(point_1, point_2)

    if dist != 0:
        app_azim = (np.asin((point_1[0] - point_2[0]) / dist) / np.pi * 180) % 179

    else:
        app_azim = 0.00

    if (azim - 22.5 <= app_azim) and (app_azim < azim + 22.5):
        return True
    
    return False

# Gives all the pairs of points separated from distance h along the given azimuth (with a tolerance)
def get_uni_dir_pairs(azim, h, h_min, points):

    pairs = [];

    for i in range(len(points)):

        for j in range(i+1, len(points)):

            if test_alignment(points[i], points[j], azim):

                dist = distance(points[i], points[j])

                dist_min = h - (h_min / 2.00);
                dist_max = h + (h_min / 2.00);

                if (dist_min < dist) and (dist <= dist_max):

                    pairs.append([points[i], points[j]])
    
    return pairs

# Gives all the pairs of points separated from distance h in every direction (with a tolerance)
def get_omni_dir_pairs(h, h_min, points):

    pairs = [];

    for i in range(len(points)):

        for j in range(i+1, len(points)):

            dist = distance(points[i], points[j])

            dist_min = h - (h_min / 2.00);
            dist_max = h + (h_min / 2.00);

            if (dist_min < dist) and (dist <= dist_max):

                pairs.append([points[i], points[j]])
    
    return pairs

# Plots two property alongside
def cross_plot(points, property_index_1, property_index_2):

    figure = plt.figure();

    for index in range(len(points)):

        plt.scatter(points[index][property_index_1], points[index][property_index_2], color='b')

    plt.show()

    # plt.close(figure)

# Compute the uni-directional variogram from set of data points
def compute_uni_dir_empirical_variogram(points, property_index, azim=0, h_min=1, nb_points=5, show=True):

    # Initializes list of variogram points
    variograms = [];

    # Computes all distances to be considered for the computation → list of considered distances h
    distances = [k * h_min for k in range(1, nb_points + 1)]

    # Computes the variogram for each distance h
    for h in distances:

        # Gets all pairs of points separated from distance h
        pairs = get_uni_dir_pairs(azim, h, h_min, points)

        # Initialize the variogram for the given distance h
        variogram = 0

        # Sum the variability of each pair of points for the given distance h
        for pair in pairs:

            # TODO
        
        # Aggregates the variogram for the given distance h to variogram points
        variograms.append([h, variogram])

    # Plots the empirical variogram for a given minimal distance, and number of points 
    if show:

        figure = plt.figure()

        h = [variograms[i][0] for i in range(len(variograms))]
        y = [variograms[i][1] for i in range(len(variograms))]

        plt.plot(h, y, '--o')

        plt.show()

        plt.close(figure)

    # Returns the empirical variogram for a given minimal distance, and number of points
    return variograms

# Compute the omni-directional variogram from set of data points
def compute_omni_dir_empirical_variogram(points, property_index, h_min=1, nb_points=5, show=True):

    # Initializes list of variogram points
    variograms = [];

    # Computes all distances to be considered for the computation
    distances = [k * h_min for k in range(1, nb_points + 1)]

    # Computes the variogram for each distance
    for h in distances:

        # Gets all pairs of points 
        pairs = get_omni_dir_pairs(h, h_min, points)

        # Initialize the variogram for the given distance
        variogram = 0

        # Sum the variability of each pair of points for the given distance
        for pair in pairs:

            # TODO

        # Aggregates the variogram for the given distance to the global variogram
        variograms.append([h, variogram])

    # Plots the empirical variogram for a given minimal distance, and number of points 
    if show:

        figure = plt.figure()

        h = [variograms[i][0] for i in range(len(variograms))]
        y = [variograms[i][1] for i in range(len(variograms))]

        plt.plot(h, y, '--o')

        plt.show()

        plt.close(figure)

    # Returns the empirical variogram for a given minimal distance, and number of points
    return variograms

# Compute the omni-directional variogram from set of data points
def compute_multi_dir_empirical_variogram(points, property_index, azim=0, h_min=1, nb_points=5, show=True):

    # Initializes list of variogram points
    multi_dir_variograms = [];

    azimuths = [azim + (k * 45) for k in range(4)]

    # Computes the omni-directional variogram for each azimuth
    for azim in azimuths:

        # Computes the uni-directional empirical variogram for the given azimuth
        # TODO

        # Aggregates the omni-directional variogram for the given azimuth to the list of multi-directional variograms
        multi_dir_variograms.append(uni_dir_variogram)

    # Plots the empirical variogram for a given minimal distance, and number of points 
    if show:

        figure, axis = plt.subplots(2, 2)

        h = [[variograms[k][i][0] for i in range(len(variograms))] for k in range(len(azimuths))]
        y = [[variograms[k][i][1] for i in range(len(variograms))] for k in range(len(azimuths))]

        plt.plot(h, y, '--o')

        axis[0, 0].plot(h[0][0], y[0][1], '--o')
        axis[0, 1].plot(h[1][0], y[1][1], '--o')
        axis[1, 0].plot(h[2][0], y[2][1], '--o')
        axis[1, 1].plot(h[3][0], y[3][1], '--o')

        axis[0, 0].set_title('Azimuth ' + str(azimuths[0]))
        axis[0, 1].set_title('Azimuth ' + str(azimuths[1]))
        axis[1, 0].set_title('Azimuth ' + str(azimuths[2]))
        axis[1, 1].set_title('Azimuth ' + str(azimuths[3]))

        plt.show()

        plt.close(figure)

    # Returns the empirical variogram for a given minimal distance, and number of points
    return multi_dir_variograms

def nugget_model(list_h, c):

    # Initializes list of y values
    list_y = []

    # Computes the variogram model for each distance h
    for h in list_h:

        # TODO    

    return list_y

def gaussian_model(list_h, a, c):

    # Initializes list of y values
    list_y = []

    # Computes the variogram model for each distance h
    for h in list_h:

        # TODO    

    return list_y

def spherical_model(list_h, a, c):
    
    # Initializes list of y values
    list_y = []

    # Computes the variogram model for each distance h
    for h in list_h:

        # TODO    

    return list_y

def exponential_model(list_h, a, c):
    
    # Initializes list of y values
    list_y = []

    # Computes the variogram model for each distance h
    for h in list_h:

        # TODO    

    return list_y

def rms(func, x, y, params):
    
    return np.mean((func(x, *params) - y)**2)

def compute_omni_dir_variogram_model(empirical_model):

    x = np.array(empirical_model[:, 0])
    y = np.array(empirical_model[:, 1])

    params_gaussian = sp.optimize.curve_fit(gaussian_model, x, y)
    params_spherical = sp.optimize.curve_fit(spherical_model, x, y)
    params_exponential = sp.optimize.curve_fit(exponential_model, x, y)

    rms_gaussian = sp.optimize.curve_fit(gaussian_model, x, y, params_gaussian[0])
    rms_spherical = sp.optimize.curve_fit(spherical_model, x, y, params_spherical[0])
    rms_exponential = sp.optimize.curve_fit(exponential_model, x, y, params_exponential[0])

    return [[rms_gaussian, "gaussian"], [rms_spherical, "spherical"], [rms_exponential, "exponential"]]

if __name__ == "__main__":

    # Enter the name of the data file
    file_name = "./poroperm_sparsedat.sec"

    # In case, update the delimiter
    file_data = np.loadtxt(file_name, delimiter='\t', skiprows=1);

    # Plot the points in a 2D map view – X property index = 0, Y property index = 1
    cross_plot(file_data, 0, 1)

    # Compute the omnidirectional variogram of the porosity (property index = 4)
    omni_dir_poro_variogram = compute_uni_dir_empirical_variogram(# TODO)