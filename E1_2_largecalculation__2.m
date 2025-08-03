% -----
% Exercise 1.2: large interdependent calculation 
% Sum 0.000001 1,000,000 times, use
% (i) single and (ii) double precision
% -----
clear all 
close all
clc
format longe   % sets the output format in the command window
               % for more info just type >> help format 
% --------
% using single precision
true_solution=1
x1=single(0);
for i=1:1000000
    x1=x1+0.000001;
end    
x1
% compute absolute error
E1 = abs(x1-true_solution)

% --------
% using double precision
true_solution=1
x2=double(0);
for i=1:1000000
    x2=x2+0.000001;
end    
x2
% compute absolute error
E2 = abs(x2-true_solution)