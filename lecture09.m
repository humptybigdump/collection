% Group 2

clear all;
close all;

N = 4;
L = 2*pi;
x_0 = [0 pi/2 pi 3*pi/2];
f = [1 1 1 1];

z = [1 exp(2*pi*1i/N) exp(4*pi*1i/N) exp(6*pi*1i/N)];

alpha_0 = 1/N*dot(f,z.^0);
alpha_1 = 1/N*dot(f,z.^1);
alpha_2 = 1/N*dot(f,z.^2);
alpha_3 = 1/N*dot(f,z.^3);

x = 0:0.01:2*pi;
f_x = alpha_2*exp(2*pi*1i*-2*x/L)+alpha_3*exp(2*pi*1i*-1*x/L)+alpha_0*exp(2*pi*1i*0*x/L)+alpha_1*exp(2*pi*1i*1*x/L)+alpha_2*exp(2*pi*1i*2*x/L);

figure;
plot(x, real(f_x), x_0, f);

%% Trigonometrische Interpolation

n = N-1;
v = 0:n;