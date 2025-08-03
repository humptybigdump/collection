import numpy as np
import matplotlib.pyplot as plt

t = np.arange(0,100,1)                      #Zeit

x = np.sin(2*np.pi*0.025*t)                 #Signal

comb = np.zeros(t.size)                     #Dirac-Kamm (Wir haben durch die diskreten Werte implizit einen Dirak
comb[::5] = np.ones(20)                     # hier tun wir nun so als waere unser Signal mit einer geringeren Abtastrate
                                            # abgetastet worden damit wir in unserem "quasi kontinuierlichen" Spektrum
                                            # die Wiederholungen sehen)

x_c = x*comb                                # Abtasten unseres Signals mit geringerer Abtastfrequenz

plt.figure()                                # Abbildung 1 (Signal und abgetastetes Signal)
plt.plot(t,x)
plt.stem(t,x_c)

x_c = np.sum(np.abs(x)**2)/np.sum(np.abs(x_c)**2)*x_c   #Skalieren damit die Spitzen im Spektrum gleich hoch sind
                                                        #Warum?

X = np.fft.fftshift(np.fft.fft(x))          #DFT berechnen und verschieben der berechneten Werte
X_c = np.fft.fftshift(np.fft.fft(x_c))      #Warum? (Tipp: Siehe numpy Dokumentation)

plt.figure()                                #Abbildung 2 (Spektrum Signal und Spektrum abgetastetes Signal)
plt.plot(np.abs(X_c)**2)
plt.plot(np.abs(X)**2)

print(np.sum(x))
print(np.sum(np.abs(x)**2)**2/2)
print(np.sum(x)**2)

h = 1/5*np.sinc((t-50)/5)
x_rec = np.convolve(h,x_c)[50:-49:1]

plt.figure()
plt.plot(t,x)
plt.stem(t,x_rec)

plt.show()


