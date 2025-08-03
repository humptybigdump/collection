# Benoetigte Module werden importiert und eingerichtet
import random, time 
import RPi.GPIO as GPIO
  
GPIO.setmode(GPIO.BCM) 
 
# Hier werden die Ausgangs-Pin deklariert, an dem die LEDs angeschlossen sind.
LED_Rot = 4
LED_Gruen = 17
LED_Blau = 18
  
# Set pins to output mode
GPIO.setup(LED_Rot, GPIO.OUT) 
GPIO.setup(LED_Gruen, GPIO.OUT)
GPIO.setup(LED_Blau, GPIO.OUT)
  
Freq = 100 #Hz
  
# Die jeweiligen Farben werden initialisiert.
ROT = GPIO.PWM(LED_Rot, Freq) 
GRUEN = GPIO.PWM(LED_Gruen, Freq)
BLAU = GPIO.PWM(LED_Blau, Freq)
ROT.start(0)  
GRUEN.start(0)
BLAU.start(0)
  
# Diese Funktion generiert die eigentliche Farbe
# Mittels der jeweiligen Farbvariable, kann die Farbintensitaet geaendert werden
# Nachdem die Farbe eingestellt wurde, wird mittels "time.sleep" die Zeit definiert,
# wie lang die besagte Farbe angezeigt werden soll
 
def LED_Farbe(Rot, Gruen,Blau, pause):
    ROT.ChangeDutyCycle(Rot)
    GRUEN.ChangeDutyCycle(Gruen)
    BLAU.ChangeDutyCycle(Blau)
    time.sleep(pause)
 
    ROT.ChangeDutyCycle(0)
    GRUEN.ChangeDutyCycle(0)
   
print ("LED-Test [druecken Sie STRG+C, um den Test zu beenden]")
  
# Hauptprogrammschleife:
# Diese hat die Aufgabe fuer jede einzelne Farbe eine eigene Variable zu erstellen
# und mittels einer For-Schleife die Farbintensitaet jeder einzelnen Farbe von 0-100% zu druchlaufen
# Durch die Mischungen der verschiedenen Helligkeitsstufen der jeweiligen Farben
# entsteht somit ein Farbverlauf
try:
    while True:
        for x in range(0,2):
            for y in range(0,2):
                for z in range(0,2):
                    print (x,y,z)
                    for i in range(0,101):
                        LED_Farbe((x*i),(y*i),(z*i),.02)
  
# Aufraeumarbeiten nachdem das Programm beendet wurde
except KeyboardInterrupt:
        GPIO.cleanup()