import os 
import numpy as np

# Set your path where you install eHDECAY
path = '/home/yourself/Desktop/eHDECAY/ehdecay/'

# Create the output files where we will save the results of the BRs from eHDECAY
outH = open('BRH-5.dat','w')

# Write the headers for the files
print('#param','BB','TAUTAU','GG','GAGA','ZGA','WW','ZZ','WIDTH',file=outH)


# The following loop goes over the values of a parameter XX and evaluates the BRs
for XX in np.linspace(0,1,101):
	# Move to the eHDECAY directory	
	os.chdir(path)
	# Open the hdecay.in file and write a new one with the wanted inputs
	og = open('ehdecay.in','r') 
	new = open('ehdecay.new.in','w')

	i = 1
	for line in og:
		if i==67: new.write('LAGPARAM = 2 \n') #  
		elif i==102: new.write('fermrepr = 2 \n')
		
		elif i==103: new.write('XX       = '+str(XX)+' \n') # Change the inputs that you want. You must add as many spaces needed to align the '=' symbols in the input file "ehdecay.in". Do not forget the floating point in your input! Do not forget the line break at the end! 
		# Be aware! If you run the script once you will change the format of the input file, so it can useful to save a copy of it
		
		else: new.write(line)
		
		i=i+1

	# Close files	
	og.close()
	new.close()

	# Substitute old hdecay.in with the new one
	os.system('mv ehdecay.new.in ehdecay.in')

	# Run eHDECAY
	os.system('./run')

	# Read the BRs from the output files

	# Resuls for H
	f1 = open('br.eff1','r')
	f2 = open('br.eff2','r')

	for i in range(3):
		f1.readline()
		f2.readline()

	f=f1.readline().split() #read from the first file
	HtoBB = f[1]
	HtoTAUTAU = f[2]

	f=f2.readline().split() #read from the second file
	HtoGG = f[1]
	HtoGAGA = f[2]
	HtoZGA = f[3]
	HtoWW = f[4]
	HtoZZ = f[5]
	TotW = f[6]

	# Print results in the output files
	print(XX,HtoBB,HtoTAUTAU,HtoGG,HtoGAGA,HtoZGA,HtoWW,HtoZZ,TotW,file=outH)
	
	# Close the files!
	f1.close()
	f2.close()

# we close the output files!
outH.close()

