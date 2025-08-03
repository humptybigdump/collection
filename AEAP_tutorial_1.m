%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   AEAP - Tutorial 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Show how to download MATLAB

% Start MATLAB and enter the commands:

1+2
1-3
2*5
5/8
5+*8 % does not work -> error message
5/7

% By default MATLAB displays 4 digits after the comma (but saves many more). To change that:
format long
5/7
format short, 5/7

% In general, we can enter several commands and separate them with a comma:
sqrt(2), 2^3, exp(1+3), log(2.8)
2+3*4 % ...so * (and /) has higher priority than + (and -)
3*2^2 % ...so ^ has higher priority than * (and /)
 
% Show the history (press "up" on keyboard), explain scripts, explain comments
 
% What is the "workspace"? Explain.
a = 5
b = 3
c = a+b;
% Output can be suppressed by entering a semicolon after the command
c
clear a
clear % clear all
clc % clear command window (does not clear the workspace)

%% Vectors and matrices
x = [1;2;3;4;5] % column vector
x' % transpose (row vector)
y = [7;8;9]
z = [x;0;y] % we can also stack vectors
z+y % does not work
length(x)
size(x)
numel(x)
 
a = 1:7
b = 1:2:7
c = 7:1
c = 7:-1:1
d = 7:-2:1
e = linspace(1,9,6)
help linspace

% we can also directly search for functions in the help, click on little 
% fx-symbol next to >> and search for "mean"
mean(z)
std(z)
% search for skewness. skewness is in the "stats" toolbox. 
% Explain toolboxes
skewness(z) 
 
% how can we select certain coefficients from a vector?
z = 11:16
z(5)
z(3:6)
z(3:end)
z(3:end-1)
z([2,4,6])
z([true,false,true,true,false,false])
z<14
z([1,1,1,0,0,0])
z(logical([1,1,1,0,0,0]))
z(z<14)
z(z>14) = 14

repmat(y,3,1) % create new vectors from old vectors
repmat(y,1,3)

%% Matrices
A = [1,2,3;4,5,6]
z
Z = reshape(z,2,3)
Z = reshape(z,2,2) % error

Z+A % usual matrix addition
Z*A % does not work, inner matrix dimensions must agree
Z*A'
Z'*A
Z.*A % componentwise matrix multiplication

A(1,3)
A(1,[1,3])
A(:,1:2)
A(2,:)

 
T = zeros(4,3)
T = ones(3,2)
E = eye(3)

clear
clc

% Solving linear equations
 
SLE = [1,2,0;0,1,0;2,0,1]
A = inv(SLE) % calculate inverse
A*SLE
SLE*A

y = [7;8;9]
x = SLE\y % explain why syntax is intuitive (y = A*x -> "divide" by A from the left)
SLE*x 

% what happens if the SLE is overdetermined (we have more equations than unknowns)
SLE = SLE(:,1:2)
x = SLE\y
SLE*x % does not fit perfectly. Reason: There is no exact solution of this SLE
% Matlab automatically runs an OLS regression. 
% x is chosen such that SLE*x is as close as possible to y (in terms of least squares)

% How can we verify this? Look for a Matlab command that performs regressions
% Google: "Matlab ols regression"
x2 = regress(y,SLE)
[b,bint,r,rint,stats] = regress(y,SLE) % check the help to understand the output
% We have only 3 observations, so this regression does not really make sense
 
%% Data

% Create a folder on your computer where you save your AEAP stuff
% Download AEAP_data_1.zip from the ILIAS page, unzip it, and save it in your new folder
% Change the directory in Matlab to the new folder. We can browse in Matlab
% directly, using the "Current Folder"
cd('C:\Users\thimme\AppData\Local\Dropbox\2_teaching\1_lectures\21_AEAP') % alternatively

% load data via "Import Data"-tool
data = csvread('AEAP_data_1.csv',1); % alternatively. The second argument is the number of lines that should be dropped (header).
% Loading other formats (xls, txt, ...) is possible too --> google

dates = data(:,1);
MKT = data(:,2);
Returns = data(:,3:end);
constant = ones(size(MKT));

% estimate market betas
[beta,bint,r,rint,stats] = regress(Returns(:,1),[constant,MKT]);
beta
bint
stats(1)

