% Script: lqr_example
%
%  Description:
%          Example implementation of an LQR for a mass-spring-damper
%          system
%
%  Specials:
%          -
%
%  Authors: Thomas Meurer (KIT)
%  Email: thomas.meurer@kit.edu
%  Website: https://www.mvm.kit.edu/dpe.php
%  Creation date: 21.01.2025
%  Last revision date: 21.01.2025
%  Last revision author: Thomas Meurer (KIT)
%
%  Copyright (c) 2025, DPE/MVM, KIT
%  All rights reserved.

clear all;
close all;

% Masse-Feder-Dämpfer-System 
k  = 1;
d  = 0.5; 
A  = [0,1;-k,-d];
b  = [0;1];
cT = [1,0];

% LQR-Entwurf und Simulation des geschlossenen Regelkreises
R = {1e-2,1.0,10.0,10.0}; 
Q = {eye(2),eye(2),eye(2),[0,0;0,100]}; 
for j=1:size(Q,2)
    % Verstärkungsmatrix
    K{j} = lqr(A,b,Q{j},R{j});
    %Simulation des geschlossenen Regelkreises
    Ag = A-b*K{j};
    bg = [0;0];
    Cg = eye(2); 
    dg = 0;
    sysg = ss(Ag,bg,Cg,dg);
    t  = linspace(0,10,201); 
    x0 = [1;0.5]; 
    y  = lsim(sysg,zeros(size(t)),t,x0);
    % Graphische Ausgabe
    figure(j); 
    subplot(1,2,1); 
    plot(t,y); grid on; 
    legend('x_1','x_2'); xlabel('t'); ylabel('x');
    subplot(1,2,2); 
    plot(t,-K{j}*y'); grid on; 
    xlabel('t'); ylabel('u');
end




