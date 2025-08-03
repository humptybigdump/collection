%STUD_MATLAB_INTRO_PART1 Introduction to Matlab/Simulink.
%
%  Description:
%          Introduction to Matlab/Simulink
%          This sample file is used to learn the basics Matlab commands. This is a so-called m-file,
%          in which the code is combined into one file and can be executed directly. Next to
%          This m-file can also be followed by the code directly in the command window
%          at the ">>" prompt and executed.
%
%          The percent sign is used in Matlab to include comments.
%          Furthermore, it is possible in the Matlab built-in editor by two consecutive percent symbols
%          to divide the source text into sections. Execute the file either by entering the file name in
%          the Command Window or by pressing the F5 key in the built-in editor. Individual sections of
%          the code can be executed by the combination CTRL+Enter. Individual lines in the program code can
%          still be marked and can be executed by pressing the F9 key.
%
%          The result of the evaluations appears in the command window. It is evaluated line by line.
%          Is the program line with terminated with a semicolon, the output is suppressed. 
%
%  Authors: Jan Reinhold (ACON Kiel), Thomas Meurer (KIT)
%  Email: thomas.meurer@kit.edu
%  Website: https://www.mvm.kit.edu/dpe.php
%  Creation date: 01.04.2019
%  Last revision date: 05.11.2024
%  Last revision author: Thomas Meurer (KIT)
%
%  Copyright (c) 2023, DPE/MVM, KIT
%  All rights reserved.

% When starting a new program, all old ones should first be started
% variables are deleted and all open windows are closed.

clear variables % All variables are deleted
close all % All windows are closed
clc % The command window is reset

%%

% Matlab help: There is help for all Matlab commands that either
% directly in the command window or in an additional help window
% is displayed (the help also contains very good ones
% introductory examples)

help sqrt % Help in the Command Window
%doc sqrt % Help in the documentation (usually more detailed)

%%

% Assignments in Matlab: Matlab uses floating point numbers by default
% (double: 64 bits).

variable_1 = 2; % The semicolon suppresses the output
variable_2 = 2.0; % No difference from before
variable_3 = 4e5 % By omitting the semicolon,
                    % the result of the line is output
variable_3 = variable_1 % Overwriting variables

% Some variables are predefined

variable_4 = pi % circle number pi
variable_5 = i % Complex unit
variable_6 = 1j % Complex unit. The 1 before the j increases robustness
variable_7 = eps % floating point precision
variable_8 = NaN % Not a number, invalid result

% Assigning string variables

variable_9 = 'Hello World' % Enter names with apostrophes
variable_10 = strcat('Hello', ' World') % Combining strings

%%

% Mathematical functions and operators

v1 = 2;
v3 = 3;

v1+v3% addition
v1*v3% multiplication
v1-v3% subtraction
v1/v3% division

mod(v1,v3) % module division
rem(v1,v3) % remainder of division

sqrt(v1) % root
exp(v1) % exponential function
log(v1) % Natural logarithm
log10(v1) % logarithm of ten

abs(v1) % amount
sign(v1) % Signum
round(v1) % rounds
ceil(v1) % Round up
floor(v1) % Round down

real(2+i) % real part
imag(2+i) % imaginary part
conj(2+i) % complex conjugation
angle(2+i) % Phase of a complex number

sin(v1) % sine
cos(v1) % cosine
tan(v1) % tangent
cot(v1) % cotangent


%%

% Input of vectors and matrices

zv = [1,2,3]% row vector
sv = [1;2;3]% column vector
zv2 = 1:1:5% row vector [1 2 3 4 5]
                     % Usage start:(increment:)end
                     % The step size parameter is optional
zv3 = 1:2:5% row vector [1 3 5]
zv4 = linspace(1,2,0.1)

zv5 = [zv zv3] % Merging vectors

% Input of matrices

matr1 = [1 2;3 4] % 2x2 matrix
matr2 = ones(3) % 3x3 ones matrix
matr3 = zeros(4,2) % 4x2 zero matrix
matr4 = randn(4) % 4x4 random matrix
matr5 = rand(4) % 4x4 random matrix with values between 0 and 1
matr6 = eye(3) % 3x3 identity matrix

% Other specified numbers, matrices and vectors can be found here
% doc elmat

% Access to entries of vectors and matrices

zv(2) % Second element of zv
zv(end) % Last element of zv
matr1(2,2) % element in the second column and the second row
length(zv) % Length of the vector zv
size(matr1) % Size of the matrix matr1
matr2(1:2,2:3) % Use certain submatrices

% Calculating with matrices and vectors

det(matr1) % determinant of the matrix
rank(matr1) % Rank of the matrix
eig(matr1) % eigenvalues of the matrix
inv(matr1) % Inverse of the matrix
matr1' % transpose of the matrix

zv*zv' % dot product
zv'*zv % Dyadic product
zv.*zv % Execute the multiplication element by element

matr1*[1;2] % matrix-vector product
matr1^2% matrix square

[min1,min2] = min(zv) % minimum and position of the minimum of a vector
[max1,max2] = max(zv) % maximum and position of the maximum of a vector
mean(zv) % Mean value of a vector
std(zv) % standard deviation from the mean of a vector
sum(zv) % Sum of vector elements
prod(zv) % product of vector elements

%%

% structures and cells

% In order to manage complex, related data clearly, we recommend
% the use of structures. Access is achieved using the
% "." operator

% Creating a structure

stru1 = struct('name','Albert','age',21) % With the struct operator
stru2.name = 'Albert' % With the dot operator
stru2.age = 129

% Access to the structure is analogous

% Nested structure

stru3.name.firstname = 'Albert'
stru3.name.lastname = 'Einstein'
stru3.age = 134

% Creating a second entry in the structure

stru3(2).name.firstname = 'Isaac'
stru3(2).name.lastname = 'Newton'
stru3(2).age = 366


% Cell arrays: Are mostly used to store different data

cell1 = cell(2,2) % E
