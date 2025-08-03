%% Ausgabe
clear all
close all
clc

v = [1 2 3];

%%
disp(v)

ausgabeString = 'Der Wert der Variablen';
disp(ausgabeString);
string1 = ['Das erste Element des Vektors v ist ' num2str(v(1,1)) '.'];
disp(string1);

% Errormeldung
error('Hier ist ein Fehler')