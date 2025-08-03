import os 
import numpy as np

# Set your path where you install HDECAY
path = '/home/fran/KIT/hdecay'

# Create the output files where we will save the results of the BRs from HDECAY
outH = open('BRH.dat','w')
outA = open('BRA.dat','w')
outHp = open('BRHp.dat','w')

# Write the headers for the files
print('#param','BB','TAUTAU','TT','GG','GAGA','ZGA','WW','ZZ','hh','AA','ZA','WHp','HpHp',file=outH)
print('#param','BB','TAUTAU','TT','GG','GAGA','ZGA','Zh','ZH','WHp ',file=outA)
print('#param','TAUNU','TB','hW','HW','AW',file=outHp) 


# The following loop goes over the values of a parameter XX and evaluates the BRs
for XX in np.linspace(XXinit,XXfinal,numberofsteps):
	# Move to the HDECAY directory	
	os.chdir(path)
	# Open the hdecay.in file and write a new one with the wanted inputs
	og = open('hdecay.in','r') 
	new = open('hdecay.new.in','w')

	i = 1
	for line in og:
		if i==8: new.write('2HDM     = 1 \n') # Always set 2HDM = 1
		elif i==62: new.write('PARAM    = 1 \n') # Always set the physical basis
		
		elif i==xx: new.write('NAMEOFXX = '+str(XX)+' \n') # Change the inputs that you want. You must add as many spaces needed to align the '=' symbols in the input file "hdecay.in". Do not forget the floating point in your input! Do not forget the line break at the end! 
		# Be aware! If you run the script once you will change the format of the input file, so it can useful to save a copy of it
		elif i==70: new.write('MHH      = '+str(XX)+ ' \n')
		elif i==71: new.write('MHA      = '+str(XX)+ ' \n')		
		elif i==72: new.write('MH+-     = '+str(XX)+ ' \n')
		
		else: new.write(line)
		
		i=i+1

	# Close files	
	og.close()
	new.close()

	# Substitute old hdecay.in with the new one
	os.system('mv hdecay.new.in hdecay.in')

	# Run HDECAY
	os.system('./run')

	# Read the BRs from the output files

	# Resuls for H
	f1 = open('br.h1_2HDM','r')
	f2 = open('br.h2_2HDM','r')
	f3 = open('br.h3_2HDM','r')

	for i in range(3):
		f1.readline()
		f2.readline()
		f3.readline()

	f=f1.readline().split() #read from the first file
	HtoBB = f[1]
	HtoTAUTAU = f[2]
	HtoTT = f[6]

	f=f2.readline().split() #read from the second file
	HtoGG = f[1]
	HtoGAGA = f[2]
	HtoZGA = f[3]
	HtoWW = f[4]
	HtoZZ = f[5]

	f=f3.readline().split() #read from the third file
	Htohh = f[1]
	HtoAA = f[2]
	HtoZA = f[3]
	HtoWHp = f[4]
	HtoHpHp = f[5]

	# Print results in the output files
	print(XX,HtoBB,HtoTAUTAU,HtoTT,HtoGG,HtoGAGA,HtoZGA,HtoWW,HtoZZ,Htohh,HtoAA,HtoZA,HtoWHp,HtoHpHp,file=outH)
	
	# Resuls for A
	f1 = open('br.a1_2HDM','r')
	f2 = open('br.a2_2HDM','r')
	f3 = open('br.a3_2HDM','r')

	for i in range(3):
		f1.readline()
		f2.readline()
		f3.readline()

	f=f1.readline().split() #read from the first file
	AtoBB = f[1]
	AtoTAUTAU = f[2]
	AtoTT = f[6]

	f=f2.readline().split() #read from the second file
	AtoGG = f[1]
	AtoGAGA = f[2]
	AtoZGA = f[3]
	AtoZh = f[4]
	AtoZH = f[5]

	f=f3.readline().split() #read from the third file
	AtoWHp = f[1]

	# Print results in the output files
	print(XX,AtoBB,AtoTAUTAU,AtoTT,AtoGG,AtoGAGA,AtoZGA,AtoZh,AtoZH,AtoWHp,file=outA)
	
	
	# Resuls for Hp
	f1 = open('br.c1_2HDM','r')
	f2 = open('br.c2_2HDM','r')
	f3 = open('br.c3_2HDM','r')

	for i in range(3):
		f1.readline()
		f2.readline()
		f3.readline()

	f=f1.readline().split() #read from the first file
	HptoTAUNU = f[2]
	HptoTB = f[6]

	f=f3.readline().split() #read from the third file
	HptohW = f[1]
	HptoHW = f[2]
	HptoAW = f[3]

	# Print results in the output files
	print(XX,HptoTAUNU,HptoTB,HptohW,HptoHW,HptoAW,file=outHp)
	
	# Close the files!
	f1.close()
	f2.close()
	f3.close()

# we close the output files!
outH.close()

