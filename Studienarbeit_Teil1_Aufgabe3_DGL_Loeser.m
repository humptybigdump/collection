% Studienarbeit Teil 1 - Scheiben, Aufgabe 3
% Teil a) Lösung LGS

clear all; close all;  clc;

% Da zu lösende GLS von Unbekannte Variable sigma_a (hier sigA) abhängt, 
% wird zur Lösung Matlab benutzt.

syms sigA         % Unbekannte Druck sigma_a auf den Dämpfungsring

sigI = 1000;      % Druck sigma_i
Et   = 210000*20; % E-Modul von Rohrwandung * Dicke t
nus  = 0.2;       % Querdehnzahl von der Rohrwandung

% Aus Hinweis bekannte Funktion der Radialen Verschiebung des
% Elastomerrings (abhängig von sigma_a und r):
% ure = (370.7-0.0749*sigA)*1/r+(0.00621-0.0000413*sigA)*r
% um eine Verschebung an bestimmter Stelle zu bestimmen, ein Wert statt r einsetzen 


% Gleichungssystem A*C = B 
% statt a und b entsprechende Werte einsetzen

A = [ a  a  a  a 
      a  a  a  a
      a  a  a  a
      a  a  a  a ];

B = [ b
      b
      b
      b ];

C = A\B;




