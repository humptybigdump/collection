# coding: utf-8

"""
This code is part of the course "Introduction to robot path planning" (Author: Bjoern Hein).
It gathers all visualizations of the investigated and explained planning algorithms.
License is based on Creative Commons: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) (pls. check: http://creativecommons.org/licenses/by-nc/4.0/)
"""

from IPBenchmark import Benchmark 
from IPEnvironment import CollisionChecker
from shapely.geometry import Point, Polygon, LineString
import shapely.affinity
import math
from math import cos, sin, pi
import numpy as np
import random


benchList = list()

# -----------------------------------------
trapField = dict()
trapField["obs1"] =   LineString([(6, 18), (6, 8), (16, 8), (16,18)]).buffer(1.0)
description = "Following the direct connection from goal to start would lead the algorithm into a trap."
benchList.append(Benchmark("Trap", CollisionChecker(trapField), [[10,15]], [[10,1]], description, 2))

# -----------------------------------------
bottleNeckField = dict()
bottleNeckField["obs1"] = LineString([(0, 13), (11, 13)]).buffer(.5)
bottleNeckField["obs2"] = LineString([(13, 13), (23,13)]).buffer(.5)
description = "Planer has to find a narrow passage."
benchList.append(Benchmark("Bottleneck", CollisionChecker(bottleNeckField), [[4,15]], [[18,1]], description, 2))

# -----------------------------------------
fatBottleNeckField = dict()
fatBottleNeckField["obs1"] = Polygon([(0, 8), (11, 8),(11, 15), (0, 15)]).buffer(.5)
fatBottleNeckField["obs2"] = Polygon([(13, 8), (24, 8),(24, 15), (13, 15)]).buffer(.5)
description = "Planer has to find a narrow passage with a significant extend."
benchList.append(Benchmark("Fat bottleneck", CollisionChecker(fatBottleNeckField), [[4,21]], [[18,1]], description, 2))

flowerField = dict()
mids=[[3,4], [16,8], [7,9], [3,18], [11,11], [10,17], [18,15], [20,20], [10,2]]
for i in range (len(mids)): 
    ending = str(i)
    mid = mids[i]
    flowerField["obs" + ending + str(1)] = LineString([(mid[0], mid[1]), (mid[0], mid[1] - 3.5)]).buffer(0.2)
    flowerField["obs" + ending + str(2)] = Point([mid[0] - 1, mid[1] - 0.5]).buffer(0.5).buffer(0.5)
    flowerField["obs" + ending + str(3)] = Point([mid[0] - 1, mid[1] + 0.5]).buffer(0.5).buffer(0.5)
    flowerField["obs" + ending + str(4)] = Point([mid[0] , mid[1] + 1]).buffer(0.5).buffer(0.5)
    flowerField["obs" + ending + str(5)] = Point([mid[0] + 1, mid[1] + 0.5]).buffer(0.5).buffer(0.5)
    flowerField["obs" + ending + str(6)] = Point([mid[0] + 1, mid[1] - 0.5]).buffer(0.5).buffer(0.5)
    flowerField["obs" + ending + str(7)] = Point([mid[0], mid[1] - 1]).buffer(0.5).buffer(0.5)
description = "Planer has to find a way around 2D flowers."
benchList.append(Benchmark("Flower Field", CollisionChecker(flowerField), [[3,11]], [[18,1]], description, 2))

pacmanScene = dict()

pacmanScene["wall1"] = Polygon([(1, 1), (1, 5), (2, 5), (2, 1)]).buffer(0)
pacmanScene["wall2"] = Polygon([(3, 1), (3, 3), (5, 3), (5, 1)]).buffer(0)
pacmanScene["wall3"] = Polygon([(6, 1), (6, 5), (7, 5), (7, 1)]).buffer(0)
pacmanScene["wall4"] = Polygon([(8, 1), (8, 3), (10, 3), (10, 1)]).buffer(0)
pacmanScene["wall5"] = Polygon([(11, 1), (11, 5), (12, 5), (12, 1)]).buffer(0)
pacmanScene["wall6"] = Polygon([(1, 6), (1, 10), (2, 10), (2, 6)]).buffer(0)
pacmanScene["wall7"] = Polygon([(3, 8), (3, 10), (5, 10), (5, 8)]).buffer(0)
pacmanScene["wall8"] = Polygon([(6, 6), (6, 10), (7, 10), (7, 6)]).buffer(0)
pacmanScene["wall9"] = Polygon([(8, 8), (8, 10), (10, 10), (10, 8)]).buffer(0)
pacmanScene["wall10"] = Polygon([(11, 6), (11, 10), (12, 10), (12, 6)]).buffer(0)

pacmanScene["path1"] = LineString([(0, 0), (13, 0)]).buffer(0.1)
pacmanScene["path2"] = LineString([(0, 11), (13, 11)]).buffer(0.1)
pacmanScene["path3"] = LineString([(0, 0), (0, 11)]).buffer(0.1)
pacmanScene["path4"] = LineString([(13, 0), (13, 11)]).buffer(0.1)

for i in range(1, 13, 2):
    for j in range(1, 11, 2):
        pacmanScene[f"dot_{i}_{j}"] = Point(i, j).buffer(0.1)

pacmanScene["power_pellet1"] = Point(1, 1).buffer(0.2)
pacmanScene["power_pellet2"] = Point(11, 1).buffer(0.2)
pacmanScene["power_pellet3"] = Point(1, 9).buffer(0.2)
pacmanScene["power_pellet4"] = Point(11, 9).buffer(0.2)

description = "Pacman scene with walls, dots and power pellets."
benchList.append(Benchmark("Pac-Man", CollisionChecker(pacmanScene), [[0.5,10.3]], [[12.5,1]], description, 2))

scene1 = dict()

center = (11.0, 11.0)  # Center of the star
outer_radius = 8.0
inner_radius = 2.5
num_points = 5

angle_between_points = 360 / (num_points * 2)
points = []
    
for i in range(num_points * 2):
    angle_deg = i * angle_between_points
    angle_rad = angle_deg * (3.14159 / 180)
        
    if i % 2 == 0:
        # Outer point
        x = center[0] + outer_radius * np.cos(angle_rad)
        y = center[1] + outer_radius * np.sin(angle_rad)
    else:
        # Inner point
        x = center[0] + inner_radius * np.cos(angle_rad)
        y = center[1] + inner_radius * np.sin(angle_rad)
        
    points.append((x, y))
    
scene1["star"] = Polygon(points)

description = "Planer has to find a way around a star obstacle."
benchList.append(Benchmark("Star Obstacle", CollisionChecker(scene1, limits = [[0.0,22.0],[0.0,22.0]]), [[5,5]], [[20,20]], description, 2))


scene2 = dict()

base_x_I, base_y_I = 5.0, 5.0
height_I, width_I = 12.0, 2.0

base_x_P, base_y_P = 13.0, 5.0
height_P, width_P, thickness_P = 12.0, 6.0, 2.0

scene2["I"] = Polygon([(base_x_I, base_y_I), (base_x_I + width_I, base_y_I), (base_x_I + width_I, base_y_I + height_I), (base_x_I, base_y_I + height_I)])


vertical = Polygon([(base_x_P, base_y_P), (base_x_P + thickness_P, base_y_P), (base_x_P + thickness_P, base_y_P + height_P), (base_x_P, base_y_P + height_P)])

# Create the upper half-circle of "P"
upper_circle = Polygon([(base_x_P + thickness_P, base_y_P + height_P), (base_x_P + thickness_P + width_P, base_y_P + height_P), (base_x_P + thickness_P + width_P, base_y_P + height_P / 2), (base_x_P + thickness_P, base_y_P + height_P / 2),])
    
# Create the inner cut-out of the upper half-circle
inner_cut = Polygon([(base_x_P + thickness_P * 2, base_y_P + height_P / 2), (base_x_P + thickness_P * 2 + (width_P - thickness_P), base_y_P + height_P / 2), (base_x_P + thickness_P * 2 + (width_P - thickness_P), base_y_P + height_P), (base_x_P + thickness_P * 2, base_y_P + height_P)])
    
scene2["P"] = vertical.union(upper_circle).difference(inner_cut)

description = "Planer has to find a way around the letters I P"
benchList.append(Benchmark("IP Obstacle", CollisionChecker(scene2, limits = [[0.0,22.0],[0.0,22.0]]), [[4,4]], [[20,20]], description, 2))


straight = dict()
straight["obs1"] = Polygon([(0, 4), (10.5, 4), (10.5, 18),(0, 18)])
straight["obs2"] = Polygon([(11.5, 4), (24, 4),(24, 18), (11.5, 18)])
description = "Planer has to find a narrow straight passage."
benchList.append(Benchmark("straight", CollisionChecker(straight), [[11,2]], [[11,20]], description, 2))


dots = dict()
dots["obs1"] = Point([(4, 4)]).buffer(1.3)
dots["obs2"] = Point([(6, 2)]).buffer(1)
dots["obs3"] = Point([(8, 16)]).buffer(3)
dots["obs4"] = Point([(4, 0)]).buffer(1.2)
dots["obs5"] = Point([(0, 4)]).buffer(1)
dots["obs6"] = Point([(16, 9)]).buffer(1.1)
dots["obs7"] = Point([(20, 19)]).buffer(1)
dots["obs8"] = Point([(3, 17)]).buffer(1)
dots["obs9"] = Point([(14, 18)]).buffer(3)
dots["obs10"] = Point([(4, 20)]).buffer(1.3)
dots["obs11"] = Point([(10, 6)]).buffer(1.3)
dots["obs12"] = Point([(18, 14)]).buffer(1)
dots["obs13"] = Point([(20, 2)]).buffer(1.1)
dots["obs14"] = Point([(10, 10)]).buffer(2.1)
dots["obs15"] = Point([(15, 5)]).buffer(1.6)
description = "Planer has to navigate a dotted landsacpe."
benchList.append(Benchmark("dots", CollisionChecker(dots), [[0,0]], [[22,22]], description, 2))


### setting constants
height_limit = 30
width_limit = 30
start_coord = [[0.75,2]]
end_coord = [[width_limit-0.75,2]]
std_path_width = 1.5
narrow_path_width = 0.3
block_size = 1.35
line_obs_width = 0.2

### environment variables (!NOTHING TO CHANGE!)
next_block_extens = block_size * 2 + narrow_path_width
cols = int((width_limit-std_path_width * 2) / next_block_extens)
rows = int((height_limit-std_path_width * 2) / next_block_extens)
limits = [[0, width_limit], [0, height_limit]]

### setting requirement check
# (width_limit-std_path_width * 2) / (block_size * 2 + narrow_path_width) ==> integer & even number for columns
assert (width_limit-std_path_width * 2) % next_block_extens == 0
assert cols % 2 == 1
# (height_limit-std_path_width * 2) / (block_size * 2 + narrow_path_width) ==> integer & even number for rows
assert (height_limit-std_path_width * 2) % next_block_extens == 0
assert rows % 2 == 1

narrow_cross = dict()

### edge border
narrow_cross["obs1"] = LineString([(0, 0), (width_limit, 0), (width_limit, height_limit)]).buffer(line_obs_width)
narrow_cross["obs2"] = LineString([(width_limit, height_limit), (0, height_limit), (0, 0)]).buffer(line_obs_width)
narrow_cross["obs3"] = LineString([(0, 0), (std_path_width, std_path_width)]).buffer(line_obs_width)
narrow_cross["obs4"] = LineString([(width_limit, 0), (width_limit-std_path_width, std_path_width)]).buffer(line_obs_width)
narrow_cross["obs5"] = LineString([(width_limit, height_limit), (width_limit-std_path_width, height_limit-std_path_width)]).buffer(line_obs_width)
narrow_cross["obs6"] = LineString([(0, height_limit), (std_path_width, height_limit-std_path_width)]).buffer(line_obs_width)
narrow_cross["obs7"] = LineString([(std_path_width,std_path_width+next_block_extens*(rows-1)/2),
                                   (std_path_width,std_path_width),
                                   (std_path_width+next_block_extens*(cols-1)/2,std_path_width)]).buffer(line_obs_width)
narrow_cross["obs8"] = LineString([(width_limit-std_path_width,std_path_width+next_block_extens*(rows-1)/2),
                                   (width_limit-std_path_width,std_path_width),
                                   (width_limit-std_path_width-next_block_extens*(cols-1)/2,std_path_width)]).buffer(line_obs_width)
narrow_cross["obs9"] = LineString([(width_limit-std_path_width,height_limit-std_path_width-next_block_extens*(rows-1)/2),
                                   (width_limit-std_path_width,height_limit-std_path_width),
                                   (width_limit-std_path_width-next_block_extens*(cols-1)/2,height_limit-std_path_width)]).buffer(line_obs_width)
narrow_cross["obs10"] = LineString([(std_path_width,height_limit-std_path_width-next_block_extens*(rows-1)/2),
                                    (std_path_width,height_limit-std_path_width),
                                    (std_path_width+next_block_extens*(cols-1)/2,height_limit-std_path_width)]).buffer(line_obs_width)

### narrow paths
for row in range(0, rows):
    for col in range(0, cols):
        xstart = std_path_width + col * next_block_extens
        ystart = std_path_width + row * next_block_extens
        narrow_cross["obs"+str(11+(col+row*cols)*4)] = Polygon([(xstart, ystart),
                                                                (xstart+block_size, ystart),
                                                                (xstart+block_size, ystart+block_size),
                                                                (xstart, ystart+block_size)])
        narrow_cross["obs"+str(12+(col+row*cols)*4)] = Polygon([(xstart+block_size+narrow_path_width, ystart),
                                                                (xstart+next_block_extens, ystart),
                                                                (xstart+next_block_extens, ystart+block_size),
                                                                (xstart+block_size+narrow_path_width, ystart+block_size)])
        narrow_cross["obs"+str(13+(col+row*cols)*4)] = Polygon([(xstart+block_size+narrow_path_width, ystart+block_size+narrow_path_width),
                                                                (xstart+next_block_extens, ystart+block_size+narrow_path_width),
                                                                (xstart+next_block_extens, ystart+next_block_extens),
                                                                (xstart+block_size+narrow_path_width, ystart+next_block_extens)])
        narrow_cross["obs"+str(14+(col+row*cols)*4)] = Polygon([(xstart, ystart+block_size+narrow_path_width),
                                                                (xstart+block_size, ystart+block_size+narrow_path_width),
                                                                (xstart+block_size, ystart+next_block_extens),
                                                                (xstart, ystart+next_block_extens)])

description = "Planer has to find a passage throught many narrow paths & edges."
benchList.append(Benchmark("Narrow Crossings", CollisionChecker(narrow_cross, limits), start_coord, end_coord, description, 2))

game = dict()
game["start"] = LineString([(18,0),(18,10)]).buffer(0.5)
game["box"] = Polygon([(1, 0), (1, 20), (20, 20), (20,0), (30,0), (30,30), (0,30), (0,0)]).buffer(.5)
game["trigger1"] = Polygon([(4,2), (4,3), (8,2)]).buffer(0.5)
game["fix1"] = Point([4.2,2.2]).buffer(0.5)
game["trigger2"] = Polygon([(12,2), (16,2), (16,3)]).buffer(0.5)
game["fix2"] = Point([15.8,2.2]).buffer(0.5)
game["bumper1"] = Point([15,18]).buffer(0.5)

random_seed = 1337
for i in range(25):
    random.seed(random_seed)
    x = random.randint(300, 1700)
    y = random.randint(500, 1800)
    game["obstacle"+str(i)] = Point([x/100,y/100]).buffer(0.5)
    random_seed += 1

description = "The game with triggers and marbles, I forget the name"

benchList.append(Benchmark("Game", CollisionChecker(game, limits = [[0.0,22.0],[0.0,22.0]]), [[4,18]], [[17,1]], description, 2))


# Load scene
import pickle
my_scene, start_point, end_point = None, None, None
with open('scene.pickle', 'rb') as handle:
    my_scene, start_point, end_point = pickle.load(handle)


description = "Unknown-Picke"

benchList.append(Benchmark("Unknown-Pickle", CollisionChecker(my_scene, limits = [[0.0,22.0],[0.0,22.0]]), [start_point], [end_point], description, 2))


