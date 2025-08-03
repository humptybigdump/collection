%% Schleifen und Bedingungen
clear all
close all
clc

%%
% for Schleife
summe = 0;
for a = 1:10
    summe = summe + a;
end
disp(num2str(summe))

% while Schleife

summe = 0;
a = 1;
while a <= 10
    summe = summe + a;
    a = a + 1;
    disp(summe)
end
summeEinfach = sum(1:10)

% if Bedingungen
for a = 1:10
    if mod(a,3) == 0
        disp(a)
    end
end