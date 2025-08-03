#!/usr/bin/env python
# coding: utf-8
# Funktionenblöcke werden über das Keyword "def" eingeleitet

def exponentiate(base,exponent):
    return base**exponent

# Funktionsaufruf
# x und y werden über die Position der eingegebenen Argumente bestimmt
a = exponentiate(7,2)
print(a)

# Alternativ kann die Eingabe auch über die Keywords erfolgen
# Dies kann bei komplexen Funktionen (mit vielen Argumenten) für eine bessere Lesbarkeit sorgen

a = exponentiate(base = 7, exponent = 2)
print(a)

# Hierbei ist die Reihenfolge egal
a = exponentiate(exponent = 2, base = 7)
print(a)

# Kann auch gemischt verwendet werden (hierbei stehen die Keywordarguments immer hinten)
a = exponentiate(7, exponent = 2)
print(a)

# Eine Funktion kann mit optionalen Parmetern definiert werden, die standardmäßig gesetzt werden

def praise(subjekt, adjektiv = "toll"):
    return f"{subjekt} ist {adjektiv}!"
    

print(praise("Mechanik"))
print(praise("Mechanik", adjektiv = "doof")) # :(

# Funktionen können über *args auch mit einer flexiblen Anzahl von Argumenten definiert werden

def checkIfEven(*numbers):
    
    print("My Input: ", numbers) # argumente werden als Tupel übergeben
    
    for number in numbers:
        if number%2 == 0:
            print(number, " is even")
        else:
            print(number, " is odd")
    
checkIfEven(2,3,6,7,9,10)    

# Umgekehrt können über * auch tupel als Argumente entpackt werden

myArguments = (2,3,6,7,22)

checkIfEven(*myArguments)

# Gleiches gilt für Keywordargumente über **kwargs

def praiseMany(**kwargs):
    
    print("My Input :", kwargs) # Keywordargumente werden als dict übergeben
    
    for key in kwargs.keys():
        print(f"{key} ist {kwargs[key]}!")

praiseMany(Mechanik = "toll", Optimierung = "fantastisch")

# Umgekehrt können über ** auch dicts als Argumente entpackt werden

myKeywordArguments = {"Mechanik": "toll", "Python": "super"}

praiseMany(**myKeywordArguments)

# Unterscheidung zwischen Funktionen und ihrem Aufruf

def sayHi():
    return "Hi!"
    
print(sayHi()) #Aufruf der Funktion
print(sayHi) #Funktion als Objekt

# Funktionen können als Objekte behandelt werden, z.B. kann man Sie in Listen und Dicts stecken

def add(x,y):
    return x+y

def substract(x,y):
    return x-y

myFunctionList = [add, substract, exponentiate] # ohne (args) als Objekt
print(myFunctionList[0](1,3)) # mit (args) für den Funktionsaufruf

myFunctionDict = {"addition": add, "subtraction": substract, "exponentiation": exponentiate}
print(myFunctionDict["subtraction"](5,2))

# Funktionen können auch an andere Funktionen übergeben werden

def callAnotherFunctionTwice(function, calls = 2, functionargs = (), functionkwargs = {}):
    
    for i in range(calls):
        result = function(*functionargs, **functionkwargs)
        print(result)

callAnotherFunctionTwice(add, functionargs = (2,3))
callAnotherFunctionTwice(praise, calls = 7, functionargs = ("Python",), functionkwargs = {"adjektiv": "klasse"})

# Python unterstützt objektorientiertes Programmieren. 
# In Objekten werden Daten und Funktionen die mit diesen Daten operieren zusammengefasst.
# Eine Klasse funktioniert als "Blaupause" für einen bestimmten Typ von Objekt und wird über den "class" Befehl eingeleitet

class Student(object):
    
    # Funktionen einer Klasse werden als Methoden bezeichnet. Diese werden wie Funktionen über "def" eingeleitet.
    # Als erstes Argument muss hier stets "self", also das Objekt selbst übergeben werden.
    
    # bei __init__() handelt es sich den Konstruktor der Klasse also die Methode die beim Instanzieren von Objekten
    # aufgerufen wird. Im Konstruktor können z.B. bestimmte Attribute des Objekts festgelegt werden.
    
    def __init__(self, name, age):
        self.name = name
        self.age = age
        self.hours_studied = 0
        self.hours_for_graduation = 100
        self.has_graduated = False
    
    def study(self,hours):
        self.hours_studied += hours
        print(f"{self.name} studied for {hours} hours")
        
    def graduate(self):
        
        if self.hours_studied < self.hours_for_graduation:
            print(f"{self.name} has to study {self.hours_for_graduation-self.hours_studied} more hours to graduate :(")
        else:
            print(f"Congratulations {self.name}, you're graduated!")
            self.has_graduated = True
    
    def sayHi(self):
        print(f"Hi, I'm {self.name}. I am {self.age} years old.")

# Instanziierung von Objekten

Jörg = Student("Jörg", 22)

# Aufruf von Methoden:
# Das argument "self", für das Objekt wird automatisch übergeben, 
# beim Aufruf einer Methode muss für dieses Argument also nichts übergeben werden

Jörg.sayHi()
Jörg.study(75)
Jörg.graduate()
print("Has he graduated?", Jörg.has_graduated)
Jörg.study(50)
Jörg.graduate()
print("Has he graduated?", Jörg.has_graduated)

# Vererbung
# Klassen können von anderen Klassen abgeleitet werden, dies nennt man Vererbung.
# Dabei erbt die Klasse die Attribute und Methoden der Mutterklasse

class KITStudent(Student): # hier wird die Mutterklasse angegeben, von der geerbt wird
    
    # Standardmäßig übernimmt die Subklasse die Methoden und Attribute der übergeordneten Klasse.
    # Sollen diese verändert/erweitert werden, müssen sie überschrieben werden.
    
    def __init__(self, name, age):
        Student.__init__(self, name , age) # ruft den Konstruktor der Student Klasse auf
        self.University = "KIT"
        self.hours_for_graduation = 200
    
    def sayHi(self):
        print(f"Hi, I'm {self.name}. I am {self.age} years old. I study at the {self.University}.")
        

Anne = KITStudent("Anne", 24)

# 

Anne.sayHi()
Anne.study(75)
Anne.graduate()
print("Is she graduated?", Anne.has_graduated)
Anne.study(150)
Anne.graduate()
print("Is she graduated?", Anne.has_graduated)

# Manchmal ist es nützlich eine abstrakte Klasse als Template für andere Klassen zu definieren,
# um die Methoden für diese festzulegen

import numpy as np

class AbstractLinearElasticStiffness(object):
    
    def __init__(self):
        raise Exception("The abstract linear elastic material can not be instantiated!")
        
    def computeStiffnessMatrix(self):
        pass # Das wollen wir an dieser Stelle noch nicht spezifizieren
    
    
class isotropicLinearElasticStiffness(AbstractLinearElasticStiffness):
    
    def __init__(self, E, nu):
        # Engineering parameters
        self.E = E
        self.nu = nu
        # Eigenvalues
        self.K = self.E/(3*(1-2*self.nu))
        self.G = self.E/(2*(1+self.nu))
        # Lame constants
        self.mu = self.G
        self.lam = self.K - 2/3*self.G
        
    def computeStiffnessMatrix(self):
        
        # Wir nehmen hier mal die Mandel-Notation (normierte Voigt Notation)
        
        C11 = self.lam + 2*self.mu
        C12 = self.lam
        C44 = 2*self.mu
        
        C = np.array([  C11, C12, C12,  0,   0,   0,
                        C12, C11, C12,  0,   0,   0,
                        C12, C12, C11,  0,   0,   0,
                          0,   0,   0, C44,  0,   0,
                          0,   0,   0,  0, C44,   0,
                          0,   0,   0,  0,  0,   C44]).reshape((6,6))
        
        return C

np.set_printoptions(precision = 2) # Zur Formatierung, kann ignoriert werden

steel_stiffness = isotropicLinearElasticStiffness( E = 210000, nu = 0.3 )
print(steel_stiffness.computeStiffnessMatrix())

# Objekte können wiederrum andere Objekte als Attribut besitzen und auf deren Methoden/Attribute zugreifen

class LinearElasticMaterial(object):
    
    def __init__(self,stiffness):
        self.stiffness = stiffness
        
    def computeStress(self,strain):
        
        stiffness_matrix = self.stiffness.computeStiffnessMatrix()
        
        return np.dot(stiffness_matrix,strain)
    
steel = LinearElasticMaterial(steel_stiffness)
strain = np.array([0.01,0.,0.,0.,0.,0.])

stress = steel.computeStress(strain)
print(stress)


# In[ ]:




