%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   AEAP - Tutorial 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Problem 1: See wikipedia page on Moore Penrose pseudoinversion 
% To perform a Moore-Penrose pseudoinversion in MATLAB, we can use the function pinv

A = eye(3);
A(3,3) = 0
inv(A)

PIA = pinv(A)

B = [1,2,3;4,5,6;5,7,9]
det(B)
IB = inv(B)
IB*B
B*IB*B
PIB = pinv(B)
B*PIB
B*PIB*B
PIB*B
PIB*B*PIB
clear

% Problem 2: OLS regression. See 11.4 in the book of Cochrane
% Compare with our notation from Chapter 4 of the lecture.

% Problem 3: Download package gmm from https://personalpages.manchester.ac.uk/staff/Alastair.Hall/GMMGUI.html
% Open Content.m and gmmest.m and go over inputs and outputs of the function.

% Problem 4:
data = csvread('AEAP_data_1.csv',1);

% Problem 5:
test_assets = data(:,3:end);
MKT = data(:,2);

% Run ols regression using regress
Alpha = nan(10,1);
Beta  = nan(10,1);
AlphaInt = nan(10,2);
BetaInt  = nan(10,2);

for n=1:10
    [b,bint,r,rint,stats] = regress(test_assets(:,n),[ones(size(MKT)),MKT]);
    Alpha(n) = b(1);
    Beta(n) = b(2);
    AlphaInt(n,:) = bint(1,:);
    BetaInt(n,:)  = bint(2,:);
end

disp('Alphas and confidence intervals:');
disp([Alpha,AlphaInt]);

disp('Betas and confidence intervals:');
disp([Beta,BetaInt]);

% Problem 6:
% Check syntax of gmmest again: Define options, data, popmom, stval, Wstart

options = optimset();

% write a function that provides moment conditions and gradient of sample moment conditions

% function [mom, gradmom] = MomentCondOLS(theta,data)
% %Moment conditions for OLS regression
% 
% MKT = data(:,2);
% test_assets = data(:,3:end);
% n = size(test_assets,2);
% alpha = theta(1:n)';
% beta = theta(n+1:end)';
% 
% % moment condition
% epsilon = test_assets - repmat(alpha,size(MKT,1),1) - MKT*beta;
% ortho   = epsilon.*repmat(MKT,1,n);
% mom     = [epsilon,ortho];
% 
% % gradient
% % Coefficient (i,j) must be derivative of sample moment condition i w.r.t. parameter j
% 
% gradmom = (-1)*[eye(n), mean(MKT)*eye(n); ...
%                 mean(MKT)*eye(n), mean(MKT.^2)*eye(n)];
% 
% end

stval = [zeros(10,1);ones(10,1)];
W     = eye(20);

[theta,~,~,~,~,~,~,~,~,conf_inter] = gmmest(options, data, 'MomentCondOLS', stval, W);

% Are the results the same?
[theta,[Alpha;Beta]] % point estimates of alphas and betas

[conf_inter,[AlphaInt;BetaInt]] % confidence intervals

% Interesting: The point estimates are always the same, confidence
% intervals are very close.

% OLS assumes that errors (epsilons) are i.i.d. that means they are not 
% correlated over time and always have the same vola.
% We can generalize this in GMM and allow for autocorrelation and
% heteroskedasticity. Check gmmest docu to find out default assumption
% about standard errors --> 'SerUnc', i.e. they assume that autocorrelation
% is equal to zero. GMM standard errors account for heteroscedasticity only.
% We can easily account for autocorrelation in GMM:

optset('gmmest','method','HACC_B');
[theta,~,~,~,~,~,~,~,~,conf_inter] = gmmest(options, data, 'MomentCondOLS', stval, W);
[theta,[Alpha;Beta]] % point estimates of alphas and betas
[conf_inter,[AlphaInt;BetaInt]] % confidence intervals


