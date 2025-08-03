print("Hello world!")

print(f"Hello world!")

# Einfache Ganzzahl
meine_variable = 5
print("Meine Variable ist: meine_variable") #falsch
print("Meine Variable ist: {meine_variable}") #falsch
print(f"Meine Variable ist: {meine_variable}") #richtig

# irrationale Gleitkommazahl
meine_variable_kommazahl = 2/3
print("Meine Variable ist: meine_variable_kommazahl") #falsch
print("Meine Variable ist: {meine_variable_kommazahl}") #falsch
print(f"Meine Variable ist: {meine_variable_kommazahl}") #richtig aber wenig Kontrolle
print(f"Meine Variable ist: {meine_variable_kommazahl:.2f}") #richtig mit viel Kontrolle
print(f"Meine Variable ist: {int(meine_variable_kommazahl)}") #richtig mit type-converion (ACHTUNG!)

# mehrere Variablen
meine_variable1 = 5
meine_variable2 = 50
meine_variable3 = 500
meine_variable4 = 5000
meine_variable5 = 50000
print(f"Meine Variablen sind: {meine_variable1} {meine_variable2} {meine_variable3} {meine_variable4} {meine_variable5}")
