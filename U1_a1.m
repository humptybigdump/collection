% MATLAB-Übung zum Profilfach "Auslegung und Bilanzierung von Mikro-
% strukturreaktoren
% Übung 1 - Aufgabe 1 

% 1a)
% 1b)
vec1 = 1:2:10;
vec2 = linspace(1,10,5);
vec3 = ones(1,5);

% 1c)
% Jeder Vektor wird mit anderem Operator allokiert
% vec1: Von 1 ausgehend wird der nächste Vektoreintrag bis 10 mit 2 addiert
% vec2: Es wird ein gleichmäßig verteilter Vektor von 1 bis 10 mit 5
% Elementen erzeugt
% vec3: Es wird eine Matrix mit den Dimensionen 1x5 generiert bei der alle
% Elemente mit 1 belegt sind
% Alle Vektoren sind Zeilenvektoren!

% 1d)
% Addition, Subtraktion sowie elementweise Multiplikation (.*) und
% elementweise Division (./) möglich
vec1 + vec2
vec1 - vec2
vec1 .* vec2
vec1 ./ vec2
% vec1*vec2: Multiplikation erzeugt Fehlermeldung: Inner Matrix dimensions
% must agree! * Operator steht für Matrizenmultiplikation
vec1 / vec2
% Division ist bei gleicher Spaltenanzahl ebenfalls möglich (Dokumentation:
% mrdivide), vec1 / vec2 gibt eine least-squares-Lösung für das Problem
% x * vec2 = vec1 zurück.
% "If A is a rectangular m-by-n matrix with m ~= n, and B is a matrix
% with n columns, then x = B/A returns a least-squares solution of the
% system of equations x*A = B

% 1e)
vec4 = vec1'

% 1f)
% In MATLAB R2018b sind alle folgenden Additionen und Multiplikationen
% zulässig! Die Operationen sind jedoch teilweise nicht besonders sinnvoll
vec1 + vec4
vec2 + vec4
vec3 + vec4

vec1 * vec4
vec2 * vec4
vec3 * vec4

vec1 .* vec4
vec2 .* vec4
vec3 .* vec4

vec4 * vec1
vec4 * vec2
vec4 * vec3

vec4 .* vec1
vec4 .* vec2
vec4 .* vec3

% 1g)
% erzeugt 1x15-Zeilenvektor
vec_all = [vec1, vec2, vec3]

% 1h)
% vec4 transponiert an vec_all anhängen
% erzeugt 1x20 Zeilenvektor
vec_all = [vec_all, vec4']

% 1i)
% erzeugt 3x5 Matrix
mat_all = [vec1; vec2; vec3]

% 1j)
% Abruf "Eintrag 1 bis 3" oder "Zeile 1, Spalte 1 bis 3"
vec_all(1:3)
vec_all(1, 1:3)

% 1k)
a = mat_all(1,1)
b = mat_all(3,1)
c = mat_all(2,2)

% 1l)
% Command Window leeren
clc
% Alle Variablen im Workspace löschen
clear all

