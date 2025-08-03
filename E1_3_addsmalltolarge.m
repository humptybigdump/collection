% -----
% Exercise 1.3: round-off error
%               add small to large
% Compute 100+1e-15
% -----
clear all 
close all
clc

% -----
%adding small to large numbers
a=100
b=1e-15
c=a+b

%check whether a is equal c ?
a==c  %!Notice : it is like no summation 
      %          operation has been performed.