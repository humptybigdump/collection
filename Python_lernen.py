# Dies ist die "Musterlösung" der Einführungsübung der Räumlichen Stochastik
print("Hello world!") #print-Befehl lautet print"Hello world!" bei python 2.x

#Grundrechenarten
4+3
22-7
3*6
16/4
13%3 #Modulorechnung

2**3 #Potenzrechnung

#Variablenzuweisung
x=12
y=6
x*y
x=x-y
x

#Wahrheitswerte
2<5
4>=2
2==7
x!=y

#Daten-Typen (Teil 1: Zahlen)
float(11)
int(10)
int(10.9)

(10.0).is_integer()
#x.is_integer() #Erzeugt Fehler
x=float(x)
x.is_integer() #Jetzt gehts

#Daten-Typen (Teil 2: Strings)
a_string="Hallo"
a_string
another_string="du"
a_string+" "+another_string +"!"

#Eingabeaufforderung
user_name= input("Wie heißt du? ")
print("Hallo "+user_name +"!")

















#%% Plot the points
import numpy as np
import matplotlib.pyplot as plt
x = np.linspace(0, 2 * np.pi, 50, endpoint=True)
y = np.sin(x)
start_x, end_x = -1, 2*np.pi + 1
start_y, end_y = -1.1, 1.1
plt.plot(x,y)
plt.xlim(start_x, end_x)
plt.ylim(start_y, end_y)
plt.show()

#%% Zufälliger Ball
import matplotlib.pyplot as plt
from scipy import stats

r=stats.uniform.rvs(loc = 0, scale = 1, size = 1)
figure, axes = plt.subplots()
draw_circle = plt.Circle((0, 0), r)

plt.xlim(-1,1)
plt.ylim(-1,1)
axes.set_aspect(1)
axes.add_artist(draw_circle)
plt.title('Circle')
plt.show()



#%% Zufälliges Dreieck
from scipy import stats
from matplotlib.patches import Polygon
import matplotlib.pyplot as plt 

p=stats.uniform.rvs(loc = 0, scale = 2*np.pi, size = 3)

figure, axes = plt.subplots()  
axes.add_patch(Polygon([[np.cos(p[0]),np.sin(p[0])],[np.cos(p[1]),np.sin(p[1])],[np.cos(p[2]),np.sin(p[2])]], closed=True,fill=True))

axes.set_xlim((-1.1,1.1)) 
axes.set_ylim((-1.1,1.1)) 
axes.set_aspect(1)
plt.show()

