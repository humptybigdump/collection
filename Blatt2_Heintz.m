%% Matlab script zu Geometrische Modelle Übungsblatt 2 Matlab-Aufgabe


clear variables;
close all;
clc;
%%
%Zuweisungen
xValues = linspace(1,20,20);
y = @ComputeSquare;
z = @ComputeSomething;
sum = @ComputeSum;

%% Graph
figure
tiledlayout('flow');            
nexttile;                       
scatter(xValues,y(xValues),'b');
hold on
scatter(xValues,z(xValues),'g');
scatter(xValues,sum(y(xValues), z(xValues)));
xlabel('x-Werte');
ylabel('Funktionswerte');
title('Alle 3 Funktionen in einem Bild');
legend('y(x) = x^2', 'z(x) = 3 * x +1', 'sum(f, g) = f + g')

%%
%Definitionen
function f = ComputeSquare(x)
    f = x.^2
end

function g = ComputeSomething(x)
    g = 3 * x +1
end

function s = ComputeSum(y, z)
    s = y + z
end