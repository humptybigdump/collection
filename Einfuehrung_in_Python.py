#!/usr/bin/env python
# coding: utf-8

# ## Datentypen
# Integer
a = 1
print(a)
print(type(a))

# Double
b = 1.
print(b)
print(type(b))

# String
string1 = 'Hello World'
string2 = "Hello World"
print(string1,string2)
print(string1 + string2)

# Int + Double = Double
c = a+b
print(c)
print(type(c))

# - Mal Geteilt - Wie gehabt
d = (c-b)/c*a
print(d)

# Potenzen
e = c**2
print(e)

# Modulo Operator
f = 24%7
print(f)


# ## Listen, Tupel und Dictionaries
# Listen

liste_1 = [1,2,3.,4,'String']
print(liste_1)

# Tupel

tupel_1 = (1,2,3.,4,'String')
print(tupel_1)

# Auf Elemente zugreifen - WICHTIG: Indizierung beginnt bei 0!

print(liste_1[4])
print(tupel_1[2])

# Slicing - man kann einfach auf mehrere Elemente einer Liste/eines Tupels zugreifen.

# Achtung: Die Syntax [i:j] ruft die Elemente i, i+1 , ... , j-1 (!) auf.
# Das kann zunächst etwas ungewohnt sein, hat aber den Vorteil, dass sich
# aus "j-i" leicht die Anzahl der Elemente ablesen lässt
# Beispiel: [2:5] -> ab Index 2 werden 3 Elemente aufgerufen

print(liste_1[2:5])
print(tupel_1[1:3])

# Liste: veränderbar, Tupel: unveränderbar

neue_Liste = [1,2,3,4]
neues_Tupel = (1,2,3,4)

neue_Liste[0] = 10
print(neue_Liste)
neue_Liste[2:4] = [22, 33]
print(neue_Liste)

# neues_Tupel[0] = 10 # <= das ergibt einen Error

# Es existieren noch viele weitere Möglichkeiten zur Manipulation von Listen

# Beispiel: Ein Element an eine Liste anhängen
liste_1.append(3.5) 
print(liste_1)

# Beispiel: Ein Element aus einer Liste entfernen
liste_1.remove('String')
print(liste_1)

# Beispiel: Liste sortieren
liste_1.sort()
print(liste_1)

# Beispiel: Listen addieren
print(liste_1 + [77, 88, 99])

# Für mehr Methoden und Beispiele: https://docs.python.org/3/tutorial/datastructures.html#more-on-lists

# Dictionaries - Listen und Tupel sind geordnete Datenobjekte (die Einträge lassen sich über den Index sortieren)
# Dictionaries sind ungeordnet, die Einträge haben keine "natürliche" Reihenfolge sondern werden jeweils Tags zugeordnet

Studenten = {
    "Anne" : 25,
    "Daniel" : 29,
    "Jürgen" : 22
    }

print(Studenten["Jürgen"])

# Eine Übersicht zur Syntax und Methoden von Dictionaries: https://www.programiz.com/python-programming/dictionary


# ## Loops
# for-loop
# In Python gibt es keine Klammern, wie in C++ und kein end, wie in Matlab. Stattdessen wird mit Tab eingerückt:
for i in range(10):
    print(i)

# while-loop

i = 0
while i < 10:
    print(i)
    i+=1


# ## Bool'sche Operationen
# If Abfrage
# In Python gibt es die üblichen Vergleichsoperatoren: <, <=, >, >=, !=, ==

for i in range(4):
    if i<2:
        print('tic')
    else:
        print('toc')

# Verknüpfung/Negation von Bedingungen über die Logische Operatoren: and, or und not

print(not 5 < 4)
print(5 < 4 and 2 > 1)
print(5 < 4 or 2 > 1)


# ## spezielle Syntax
# Über for loops kann man in Python elegant über die Elemente einer Liste (oder anderer iterierbarer Objekte) iterieren

# klassisch:
for i in range(len(liste_1)):
    print(liste_1[i]**2)

print("")
# python:
for element in liste_1:
    print(element**2)

# Für das Füllen/Erstellen von Listen existiert ebenfalls eine einfache/gut lesbare Syntax

# klassisch:
liste_2 = []
for i in range(10):
    liste_2.append(i)
print(liste_2)

# python:
liste_3 = [i for i in range(10)]
print(liste_3)

# Das kann auch mit if Bedingungen verknüpft werden

liste_4 = [element for element in liste_3 if element%2 == 0]
print(liste_4)

