%% Erstellung von Diagrammen
clear all
close all
clc

%%
x = 0:2/50:2;
y1 = sin(x*pi);
y2 = cos(x*pi);

figure;
plot(x, y1);

xlabel('Zeit in s');
ylabel('Spannung in V');
title('Sinus und Kosinus');

hold on
plot(x,y2, 'linewidth', 5)

set(gca, 'FontSize', 20)

%Subplot
figure;
subplot(2,1,1)
plot(x, y1)
xlabel('Zeit in s');
ylabel('Spannung in V');
title('Sinus');

subplot(2,1,2)
plot(x,y2)
xlabel('Zeit in s');
ylabel('Spannung in V');
title('Kosinus');

yyaxis right;
hold on
plot(x,y1)