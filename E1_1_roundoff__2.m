% -----
% Exercise 1.1: round-off error
% Compute : (a) subtract 0.1 ten times from 1
%           (b) subtract 0.5 twenty times from 10
% -----
clear all
close all
clc
format longe   % sets the output format in the command window
%for more info just type >> help format
%-------------------------
% First problem to solve:
% subtract 0.1 ten times from 1
true_solution=0;
a=1;
% loop for subtracting 0.1 ten times
for i=1:10
    a=a-0.1;
end
a==true_solution
% the above statement will give :
% 1 if the statement is true, and
% 0 if the statement is false

if(a==true_solution)
    disp('Result is exact.')
else
    disp('Result has an absolute error of')
    E=abs(a-true_solution)
end

%--------------------------
% Second problem to solve:
% subtract 0.5 twenty times from 20
true_solution=0;
b=10;
% loop for subtracting 0.5 twenty times from 10
for i=1:20
    b=b-0.5;
end
b==true_solution

if(b==true_solution)
    disp('Result is exact.')
else
    disp('Result has an absolute error of')
    E=abs(b-true_solution)
end

%-----------------------
% Notice that 0.1 is not a machine number,
% while 0.5 is a machine number (i.e. it can
% be represented exactly by the computer).