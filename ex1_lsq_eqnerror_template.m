 % Script: ex1_lsq_eqnerror
%
%  Description:
%          LSQ using generalized equation error for the system defined in ex1_lsq_eqnerror_init.m 
%
%  Specials:
%          Use a nominal solution for MATLAB Grader in ILIAS
%
%  Authors: Thomas Meurer (KIT)
%  Email: thomas.meurer@kit.edu
%  Website: https://www.mvm.kit.edu/dpe.php
%  Creation date: 06.11.2023
%  Last revision date: 05.11.2024
%  Last revision author: Thomas Meurer (KIT)
%
%  Copyright (c) 2024, DPE/MVM, KIT
%  All rights reserved.

clear all;
close all;

% ----------------------------------------------------------------------------------------------
% Identification

% Load the available input / output data
load('ex1_lsq_eqnerror_data1_step.mat');

% Prepare the ARX model structure
% "PLAY" with these settings!
na = 4;
nb = 3;
N  = length(t); 

% Set up the linear system using an ARX model structure in terms of the generalized equation error
S = zeros(N,na+nb+1);
for j=0:1:N-1
    sy = zeros(1,na);
    su = zeros(1,nb+1);
    %
    % to be filled
    %
    S(j+1,:) = [sy,su];
end

% Determine the coefficients of the ARX model
phat = pinv(S)*y(1:N);

% Create the corresponding z-transfer function 
Gest = tf(phat(na+1:end)',[1,-phat(1:na)'],p.Ts);

% ----------------------------------------------------------------------------------------------
% Comparison and prediction

% Compare
[yest,test] = step(Gest,t(end));
figure(1); 
%
subplot(2,1,1); hold on
stairs(t,y,'b-');
stairs(test,yest,'r-');
%
subplot(2,1,2);
stairs(t,y-yest); 

%Predict behavior for other data set
load('ex1_lsq_eqnerror_data2_step.mat');
ypred = lsim(Gest,u2,t2);
figure(2); 
%
subplot(2,1,1); hold on
stairs(t2,y2,'b-');
stairs(t2,ypred,'r-');
%
subplot(2,1,2);
stairs(t2,y2-ypred); 
