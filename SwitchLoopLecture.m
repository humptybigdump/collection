%% ===================== SWITCH CASE ===================== %%
% Evaluates an expression and chooses to execute one of several groups 
% of statements. Each choice is a case.
clear all; clc; 
n = input('Are you a student?: ');
% Without command window input
% n = true;

switch n
    case 1
        disp('You are a student')
    case 0
        disp('You are a teacher')
    otherwise
        disp('Please answer with 1 or 0')
end

%% ===================== Mini assignment ===================== %%

clear all; clc;
n = input('What is the grade?: ');
switch n
    case 1
        disp('Excellent');
    case 2
        disp('Very good');
    case 3
        disp('Good');
    case 4
        disp('Average');
    case 5
        disp('Poor');
end