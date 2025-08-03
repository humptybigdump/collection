# -*- coding: utf-8 -*-
"""
Created on Mon Oct 28 09:02:39 2024

@author: Emmanuel
"""

# convert a distance in feet to meters or vice-versa and show the result and the original value	 

# prepare the data (input sequence)
#	ask the user to give the distance, put answer in variable dist

dist = float(input("Please enter a distance in feet or meters: "))
#	ask the user to give the units, put answer in variable units
units = input("Please enter the units: ft or m: ")

#	create variable for the conversion unit: m_over_ft = 0.3048
m_over_ft = 0.3048

# process the data (processing sequence)
#	check the units of the input data, convert accordingly and define output units

if units == "ft":
    distout = dist*m_over_ft
    unitsout = "m"
else:
    distout = dist/m_over_ft
    unitsout = "ft"

# give the results (output sequence)
#	print on screen dist units = outdist outunits

print("{:f} {:s} = {:f} {:s}".format(dist, units, distout, unitsout))
# another possibility
print(f"{dist} {units} = {distout} {unitsout}")













