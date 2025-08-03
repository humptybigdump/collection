%STUD_MATLAB_INTRO_PART2 Introduction to Matlab/Simulink.
%
%  Description:
%          Introduction to the functionality of the Control Toolbox of Matlab.
%
%  Authors: Jan Reinhold (ACON Kiel), Thomas Meurer (KIT)
%  Email: thomas.meurer@kit.edu
%  Website: https://www.mvm.kit.edu/dpe.php
%  Creation date: 01.04.2019
%  Last revision date: 05.11.2024
%  Last revision author: Thomas Meurer (KIT)
%
%  Copyright (c) 2023, DPE/MVM, KIT
%  All rights reserved.


clear variables
close all
clc

%%
help control % Important: Help for the Control System Toolbox

%%

% Creating data objects in the CST
% A simple PT2 element is considered (see lecture RT I)

T = 0.5;
xi = 0.2;
V = 2;

% 1st option: transfer function

G_tf = tf([V],[T^2 2*xi*T 1])

% access to the data object

G_tf.num{1} % counter
G_tf.den{1}% denominator
G_tf.Variable % Variable

% Alternative (to be used preferably)

s = tf('s') % Create the complex variable s
G_tf1 = V/(1+2*xi*T*s+s^2*T^2)


% 2nd option: via pole zeros

p1 = (-xi+sqrt(xi^2-1))/T;
p2 = (-xi-sqrt(xi^2-1))/T;
G_zp = zpk([],[p1 p2],V/T^2)

% access to the data object

G_zp.z{1} % zero
G_zp.p{1} % pole positions
G_zp.k % gain factor
G_zp.Variable % variable


% 3rd option: state space representation

A = [0 1; -1/T^2 -2*xi/T];
b = [0;V/T^2];
c = [1;0];
d = 0;

sys = ss(A,b,c',d)


% access to the data object

sys.a % dynamic matrix
sys.b % input matrix
sys.c % output matrix
sys.d % switching matrix
sys.StateName % Name of the states


% 4th option FRD models: Numerical input of measured ones
% transfer functions, will not be discussed in more detail here

% Converting the objects into other forms of representation

G_ss2tf = tf(sys)
G_spk2tf = tf(G_zp)

sys_zpk2ss = ss(G_zp)
sys_tf2ss = ss(G_tf)

G_ss2zpk = zpk(sys)
G_tf2zpk = zpk(G_tf)

% Additional options and information
help ss2tf
help ss2zp
help zp2tf
help zp2ss
help tf2zp
help tf2ss

%%
% Discrete models
% These can in principle be created as before, but they still have to
% a sampling time can be specified

Ta = 0.2; % Specification of a sampling time

% Direct creation

z = tf('z',Ta)
Gzt = 1/(z-0.5); % As a transfer function
sysd = ss(1,1,1,0,Ta) % As state space (here: discrete integrator)

% From existing continuous model

Gz = c2d(G_tf,Ta,'zoh') % A zero-order holding element is used here
sys_z = c2d(sys,Ta,'zoh')

% models in the Tustin area (q area)

Gq = d2c(Gz,'tustin')
sys_q = d2c(sys_z,'tustin')
%%

% models with dead times

G_tzi = tf(1,[1 1],'inputdelay',0.5) % input delay
G_tzo = tf(1,[1 1],'outputdelay',0.5) % output delay
G_tzio = tf(1,[1 1],'iodelay',0.5) % input output delay


%%

% linking models
G2 = 1/s;

Gsum = G_tf+G2% sum of models
Gdiff = G_tf-G2 % difference between models
Gprod = G_tf*G2 % switching the models one after the other
Gfb = feedback(G_tf,G2) % feedback

%%

% Analysis of LTI models

dcgain(G_tf) % Determine gain factor
damp(G_tf) % Natural frequencies and attenuations
bandwidth(G_tf) % bandwidth

pole(G_tf) % Poles of the transfer function
zero(G_tf) % Zeroing the transfer function

pzmap(G_tf*(s-.1)) % Pole zero diagram
grid on

figure
impulse(G_tf,10) % impulse response for 10 seconds
hold on
step(G_tf,10) % step response for 10 seconds

% System response to any input signal

[u,t] = generous('square',1,10); % Generation of a test signal
[y,t] = lsim(G_tf,u,t); % Simulation of the system with the input signal

figure
plot(t,u,t,y)
grid on

% solution in steady state

H = freqresp(G_tf,2) % evaluation at point omega = 2

% Bode diagram

figure
bode(G_tf)
[abs,pha,omega] = bode(G_tf); % Reading the data from the Bode diagram
[abs,pha,omega] = bode(G_tf,2); % Bode diagram at the point omega = 2

% Nyquist chart

figure
nyquist(G_tf)
grid on

% order reduction of systems

minreal((s+1)/(s+1)/s) % Perform pole zero cuts

%%

% controller and observer design: In this introduction only the
% basic principles for controller and observer design are mentioned,
% the application will then take place in the later chapters

% open loop analysis

% characteristic values for the open circuit in the frequency characteristic method
allmargin(G_tf/s)


A = [-1 2; 0 -2];
b = [0;1];
c = [1;0];
d = 0;

sysr = ss(A,b,c',d)

ctrb(A,b) % accessibility matrix
obsv(A,c') % observability matrix

gram(sysr,'c') % Gram reachability matrix
gram(sysr,'o') % Gram observability matrix

acker(A,b,[-10 -11]) % pole default
place(A,b,[-10 -11]) % pole default
