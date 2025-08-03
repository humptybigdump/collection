import numpy as np
import matplotlib.pyplot as plt
import SimpleITK as sitk
import multiprocessing as mp
from timeit import default_timer as timer


def periodicDistanceVector(p1, p2, dims,periodic=True):

        dp = p1-p2
        if periodic:
            dp = dp-np.rint(dp/dims)*dims
        return dp
def writeMha(array, points, filename):
    # for point in points:
    #     array[list(point)]=-1
    image = sitk.GetImageFromArray(array.T)
    image.SetSpacing(np.array([1., 1., 1.]))
    sitk.WriteImage(image, filename, True)
def writePlt(res_array,points):
    for point in points:
        plt.scatter(point[0],point[1])
    plt.imshow(res_array.T)
    plt.xlim(0, dims[0])
    plt.ylim(0, dims[1])
    plt.show()
def get_point(current_point,points,dims,periodic):
    min_point = 0
    min_dist = np.linalg.norm(periodicDistanceVector(current_point,points[0],dims,periodic))
    for k in range(1,N):
        this_dist = np.linalg.norm(periodicDistanceVector(current_point, points[k],dims,periodic))
        if this_dist<min_dist:
            min_dist = this_dist
            min_point=k
            
    return min_point
def voro_layer(i, points, dims, periodic):
    res_array = np.zeros(dims[1:])
    weights = np.zeros(len(points))
    
    if len(dims)==2:
        
        for j in range(dims[1]):


            current_point=np.array([i,j])
            min_point = get_point(current_point,points,dims,periodic)
            #print(f"minimum_point is {min_point}")
            #res_array[i,j]=min_point
            res_array[j] = min_point
    elif len(dims)==3:
        # for i in range(dims[0]):
        print(f'{i/dims[0]*100}% finished')
        for j in range(dims[1]):
            for k in range(dims[2]):

                current_point = np.array([i,j,k])
                min_point_color= get_point(current_point,points,dims,periodic)
                res_array[j,k]=min_point_color
                weights[min_point_color]+=1
                    

    return [res_array, weights]


if __name__=='__main__':
    
    Nd = 150
    #dims = np.array([Nd,Nd,Nd])
    dims = np.array([Nd, Nd])
    periodic=True
    parallel = True
    N = 50
    points = []
    for i in range(N):
        point=[]
        for k in range(len(dims)):
            point.append(np.random.random()*dims[k])
        points.append(np.array(point))
    start = timer()
    if parallel:
        with mp.Pool() as pool:
            layers, weights = zip(*pool.starmap(voro_layer,  ((i,points, dims, periodic) for i in range(dims[0])) ))
    else:
        layers = []
        for i in range(dims[0]):
            layer, volume = voro_layer(i,points,dims,periodic)
            layers.append(layer )
    res_array = np.array(layers)
    volumes = np.sum(np.array(weights),axis=0)
    
    print(f'Volumes: {volumes}')
    plt.hist(volumes, bins=15)
    plt.show()
    print(f'Runtime: {timer()-start}s')
    # res_array, points = voro(N,dims,periodic)
    
    if len(dims)==2:
        writePlt(res_array,points)
    else:
        writeMha(res_array,points,f'test_{N}.mha')
    

