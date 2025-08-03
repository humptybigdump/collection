% Varianzfortpflanzung
% Beispiel Dreieck
clear variables
close all
clc
format long

b = 89.248;
c = 10.012;
alpha_gon = 93.641; %gon
alpha = alpha_gon/200*pi;

sig_s = 0.002;
sig_w_gon = 0.002;
sig_w = sig_w_gon/200*pi;

%% Funktionaler Zusammenhang

F = 1/2*b*c*sin(alpha)
U = b + c + sqrt(b^2+c^2-2*b*c*cos(alpha))

A_11 = c*sin(alpha)/2;
A_12 = b*sin(alpha)/2;
A_13 = b*c*cos(alpha)/2;
A_21 = 1 + (b-c*cos(alpha))/sqrt(b^2+c^2-2*b*c*cos(alpha));
A_22 = 1 + (c-b*cos(alpha))/sqrt(b^2+c^2-2*b*c*cos(alpha));
A_23 = b*c*sin(alpha)/sqrt(b^2+c^2-2*b*c*cos(alpha));

A = [A_11, A_12, A_13; A_21, A_22, A_23]

%% Varianz-Kovarianzmatrix der Eingangsgroessen

C_xx = diag([sig_s^2, sig_s^2, sig_w^2])

%% Varianzfortpflanzung

C_yy = A*C_xx*A'
sig_F = sqrt(C_yy(1,1))
sig_U = sqrt(C_yy(2,2))

% Korrelation
rho = C_yy(1,2)/(sig_F*sig_U)