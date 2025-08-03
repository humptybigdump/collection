clear all
close all
clc
warning off all

t=0:0.01:20; % time
fa=10;       % frequency a
fb=10.1;       % frequency b
A=sin(fa*t); % sine wave A
B=sin(fb*t); % sine wave B

figure
subplot(2,1,1)
plot(t,A,'r')
hold on
plot(t,B,'b')
xlabel('t')
ylabel('A, B')
legend('A','B')
subplot(2,1,2)
plot(t,A+B,'k')
xlabel('t')
ylabel('A+B')
legend('A+B')
