import re
import subprocess
import numpy as np

# This code writes and runs a script to
# run MadGraph 
# and write a file with the xs
    
scname='script.txt' # name of the script that MG will execute
launchable = 'myrun2' # name of the MG output folder
MGfile='aux.txt'
model='sm' # model you want to consider
process='e+ e- > h h z' # process you want to compute
path='' # path if you want to save the data in a specific path (this folder must exist!)

outfile=launchable+'.dat'
script=open(scname,'w')
script.write('import model '+model+' -modelname\n')
script.write('generate '+process+'\n')
script.write('output '+launchable+'\n')
    
sqrtslist = [500,1000,1500,3000,10000] 
for sqrts in sqrtslist: # you can define all the loops you want that goes over any variable of the runcard and/or the paramcard
    
    # write script for MG5
    script.write('set automatic_html_opening False\n') # do not pop the browser page 
    script.write('launch '+launchable+'\n')
    script.write('set ebeam1 '+str(float(sqrts)/2.)+'\n')
    script.write('set ebeam2 '+str(float(sqrts)/2.)+'\n')

    #script.write('set nevents '+str(nevents)+'\n') # you can increase the number of events simulated to perform the phase space integral, which leads to a larger sensitivity, but the default 10000 events should be enough

    #script.write('set xxx '+str(xxxvalue)+'\n') # with this kind of commands you can change the parameter xxx in the paramcard or the runcard indistinctively 

script.write('launch -i \n')
#change location of output file
script.write('print_results --path=./'+path+MGfile+' --format=short') # prints the results in MGfile
script.close()


# run MG5 with the script
p=subprocess.Popen('python3.10 ./bin/mg5_aMC '+scname, shell = True) # change the version of python accordingly to your pc
p.wait()

# write definitive output file to plot nice things
MGout=np.genfromtxt(path+MGfile,delimiter=' ')
# multiply by 1000 to get fb
xs=MGout[:,2]*1000 
xserr=MGout[:,3]*1000

finaldata=np.transpose([sqrtslist,xs,xserr]) # save the output data in a matrix, if you change the variable in the loop you should also change the variable here

finalfile=open(path+outfile,'w+')
np.savetxt(finalfile,finaldata, header='sqrt(s) xs xserr') # save the data in a final file, remember to change the header accordingly!
finalfile.close()




