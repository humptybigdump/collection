% -----
% Exercise 1.3: round-off error
%               add small to large
% Compute 100+1e-15
% -----
clear all 
close all
clc
format longe   % sets the output format in the command window

% -----
%adding small to large numbers
clear all
a=100
b=1e-15
c=a+b
% Notice : it is like no summation 
%          operation has been performed.
