%% Bearbeitungsbogen
clear all
close all
clc

%% Parameter
l = 'ausfuellen';                                                           % Laenge l
g = 'ausfuellen';                                                           % Erdbeschleunigung
omega = 'ausfuellen';                                                       % Rotationsgeschwindigkeit
AnzahlSchritte = 'ausfuellen';                                              % Anzahl der Zeitschritte
h = 'ausfuellen';                                                           % Zeitschrittweite

x_0 = 'ausfuellen';                                                         % Anfangsbedingungen in Zustandsvektor
t = 'ausfuellen';                                                           % Zeitvektor

x_Eu_expl(:,1) = 'ausfuellen';                                              % Anfangsbedingungen fuer Euler explizit
x_Eu_impl(:,1) = 'ausfuellen';                                              % Anfangsbedingungen fuer Euler implizit
x_RuKu(:,1) = 'ausfuellen';                                                 % Anfangsbedingungen fuer Runge-Kutta-Verfahren

%% Aufgabe 1: Systemmatrix
SystMatr = 'ausfuellen';                                                    % Systemmatrix

%% Aufgabe 2: exakte Loesung
x_exakt = 'ausfuellen';

for n = 'ausfuellen'
    
    %% Aufgabe 3: Euler explizit
    x_Eu_expl(:,n+1) = 'ausfuellen';
    
    %% Aufgabe 4: Euler implizit
    x_Eu_impl(:,n+1) = 'ausfuellen';
    
    %% Aufgabe 5: Runge-Kutta-Verfahren
    k1(:,n) = 'ausfuellen';
    k2(:,n) = 'ausfuellen';
    k3(:,n) = 'ausfuellen';
    k4(:,n) = 'ausfuellen';
    x_RuKu(:,n+1) = 'ausfuellen';
    
end

%% Aufgabe 6
l_Eu_expl = 'ausfuellen';
l_Eu_impl = 'ausfuellen';
l_RuKu = 'ausfuellen';

e_Eu_expl = 'ausfuellen';
e_Eu_impl = 'ausfuellen';
e_RuKu = 'ausfuellen';

%% Aufgabe 7: Plots
'ausfuellen';

%% Aufgabe 8: Plots
'ausfuellen';