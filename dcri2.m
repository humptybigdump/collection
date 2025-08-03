% Titlle: Quadratic-Linear Programming for Mean Variance Portfolio Analysis
% Function Name: dcri1.m
%The second formulation of the criterion function for mean-variance
%portfolio selection model, juts minize the variance subject to a contraint
%on the portfolio return
%Defines the overall variance costs of portfolio to be minimized.

function [z] = dcri2(y, P) 

global mu; global sigma;
THETA=P(1);
BETA=P(2);
N=P(3);

z=0;
for i=1:N;
    temp=0;
    for j=1:N;
        temp=temp+BETA*sigma(i,j)*y(j);
    end;
    z=z+0.5*y(i)*temp;
end;

compact formulation