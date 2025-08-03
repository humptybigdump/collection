% This is a short introduction script for MATLAB 
% and serves as a preperation with some important commands
% for the course Estimator and Observer Design
% Jerono, 2023

%- 1.0 Comments -%
% Comments are included by the "%"-sign

%- 1.1 Running a script -%
% Scripts can be executed by either the green "Run" buttom from the GUI
% or by pressing "F5"
% You can also mark specific parts of a code and only execute these by
% pressing "F9"

%- 1.2 Clear workspace and plots -%
% At the beginning of a new program, the work space should be cleared
clear all   % All variables from the workspace will be cleared
close all   % All open plots will be closed
clc         % The command window will be cleared

%- 1.3 Help function -%
% There are additional explanations to all Matlab commands
% These are available via "help command name"
help sqrt   % Explanation of the sqrt function calculating the square root
            % The Documentation can be excessed by "doc sqrt"
            % Alternatively, the web provides a lot of support, too

%- 1.4 Declaring variables -%
% By default Matlab uses floating-point (double: 64 Bit).
variable_1 = 2;         % "Semicolon" is suppresing the output in the cmd window
variable_2 = 2.0;       % Declaring a second variable
variable_3 = 4e5        % Leaving ";" the variable will be printed in the cmd
variable_3 = variable_1 % You can also overwrite existing variables

% Some variables are already predefined
variable_4 = pi    % Pi constant
variable_5 = i     % Imaginary part of complex number
variable_6 = j     % Imaginary part of complex number
variable_7 = eps   % Floating-point accuracy
variable_8 = NaN   % Not a number

% Declaring strings
variable_9  = 'Hello World'             % Strings are declared by ' '
variable_10 = strcat('Hello', ' World') % Combining strings

% Declaring structs
p.v1 = variable_1  % Structs are declared by the "." notation
p.v2 = variable_2  % Variables or vectors/matrices can be asigned to structs

%- 1.5 Mathematical functions and operations -%
v1 = 2;         % Declaring some variables
v3 = 3;
 
v1+v3           % Summation
v1*v3           % Multiplication
v1-v3           % Subtraction
v1/v3           % Division

sqrt(v1)        % Square root
abs(v1)         % Absolute value
exp(v1)         % Exponential
log(v1)         % Natural logarithm
sin(v1)         % Sinus
cos(v1)         % Cosinus

%- 1.6 Vectors and matrices -%
% Vectors
rv  = [1,2,3]           % Row vector (you can also skip the commas)
cv  = [1;2;3]           % Column vector
rv2 = 1:1:5             % Row vector [1 2 3 4 5] (start:spacing:end) 
rv3 = 1:2:5             % Row vector [1 3 5]     (start:spacing:end) 
rv4 = linspace(1,2,11)  % Row vector from 1 to 2 with 11 equally spaced elements

rv5 = [rv rv3]          % Combining vectors

% Matrices
matr1 = [1 2;3 4]   % 2x2 Matrix
matr2 = ones(3)     % 3x3 Matrix with all elements equal to 1
matr3 = zeros(4,2)  % 4x2 Zero matrix
matr4 = randn(4)    % 4x4 Matrix with gaussian distributed random variables (zero mean and var=1)
matr6 = eye(2)      % 2x2 Unit matrix

% Accessing elements of vectors and matrices
rv(2)               % Second element of rv
rv(end)             % Last element of rv
matr1(2,2)          % Element in second row and second column, respectively
length(rv)          % Length of the vector rv
size(matr1)         % Size of the matrix matr1
matr2(1:2,2:3)      % Submatrix from row 1 to 2 and column 2 to 3
matr4(:,1)          % All elements of the first column

%- 1.7 Matrix and vector operations -%
det(matr1)          % Determinant of a matrix
rank(matr1)         % Rank of a matrix
eig(matr1)          % Eigenvalues of a matrix
inv(matr1)          % Inverse of a matrix
sum(rv)             % Sum of all vector elements
matr1'              % Transpose of a matrix
rv*rv'              % Scalar product of two vectors
rv.*rv              % Elementwise multiplication
matr1*[1;2]         % Vector matrix multiplication
matr1^2             % Square of a matrix
matr1/matr6         % Matrix division (matr1 * matr6^{-1})
matr1\matr6         % Matrix division (matr1^{-1} * matr6)

%- 1.8 Stochastical properties of matrices and vectors -%
[min1,arg1] = min(rv) % Value and element number of the minimum of a vector
[max2,arg2] = max(rv) % Value and element number of the maximum of a vector
mean(rv)              % Mean value of elements of a vector
std(rv)               % Standard deviation of elements of a vector
var(rv)               % Variance of elements of a vector
mean(randn(1,1000))   % Sampled mean of 1000 elements from normalized gaussian distribution
var(randn(1,1000))    % Sampled variance ...

%- 1.9 Loops and if requests -%
var = 3;              % Declaring a variable

if var > 1            % If request with condition var > 1
    {    
        sprintf('variable value %d > 1',var)
        % Leads to text in cmd where "%d" replaces the value of var (double)
    }
else                  % Condition: everything else
    {
        sprintf('variable value %d < 1',var)
    }
end

% If-requests can be combined by
% "&&" leading to both conditions have to hold 
% or "||" leading to only of the conditions has to hold
if ((var > 1) && (var<3)) 
    { 
        sprintf('Variable satisfies 1 < var < 3')
    }
elseif (var>3)
    {
        sprintf('Variable satisfies var > 3')
    }
else
    {
        sprintf('Variable satisfies var < 1')
    }
end

% Cases 
switch var 
    case 1 
        a=1, 
    case{3,4,5} 
        a=5, 
    otherwise 
        a=10, 
end

% For loop
for i=1:2:9      % From i=1 to 9 in steps of 2, i.e., i=1,3,5,7,9
    sprintf(strcat('Current index is: ',num2str(i))) 
                 % num2str converts a number into a string 
    pause(1)     % Leads to a pause of one second in every loop
end

% While loops
k=0;             % Declare loop variable
while k<5
    sprintf(strcat('Current index is: ',num2str(k)))
                % num2str converts a number into a string 
    pause(1)    % Leads to a pause of one second in every loop 
    k=k+2;      % Set new counter value
end

%- 2.0 Timers -%
tic             % Sets the starting point of a timer    
toc             % Sets the end point of a timer

% The timer is especially usefull when one is interessted in speeding up
% the code and for identifying calculation heavy parts of the algorithm
tic
pause(1)
toc

%- 2.1 Plots -%
x  = 0:0.01:2*pi;                % Create some data y1 and y2
y1 = sin(2*x).*exp(-x/pi);       % for x from 0 to 2*pi in steps of 0.01
y2 = cos(2*x).*exp(-x/pi);

% 2D-Plots
figure('Name','2D-Plot')  % Declares figure environment
plot(x,y1)                       % Plot y1 over x 
% first argument:  x-axis
% second argument: y-axis
grid on                          % Displays a grid
hold on                          % Holds plot for second data (i.e. not overwriting)
plot(x,y2,'--','LineWidth',1)    % Plots y2 data dashed with line width of 1
xlabel('x-Data','interpreter'... % Displays the x-label in modern pc font
    ,'latex')                    % The "..." bind the next code line to the previous
ylabel('y-Data','interpreter'... % Displays the y-label in modern pc font
    ,'latex')
legend('$y_1$','$y_2$','Interpreter','latex','Location','NorthEast')
% Plots a legend inlucding the related colors of y1 and y2, respectively

% 3D-Plots
% Plot a line in R^3
x  = x(1:2:end);                 % Downsample x to reduce graphical load
figure('Name','3D-Plot')
plot3(sin(x*4*pi),cos(x*4*pi),x,'LineWidth',2)
box on    % Frames the axis
grid on   % Displays a grid
xlabel('x-Data','interpreter'... % Displays the x-label in latex font
    ,'latex')                    % The "..." bind the next code line to the previous
ylabel('y-Data','interpreter'... % Displays the y-label ...
    ,'latex')
zlabel('z-Data','interpreter'... % Displays the y-label ...
    ,'latex')

% Surface plots 
% Produce matrix of coordinates
[xx,yy] = meshgrid(x,x);  
% Create some output data
zz = sin(4*pi*xx).*cos(4*pi*yy).*exp(-xx); 

% Surface plot
figure('Name','Surf-Plot')
sp = surf(xx,yy,zz);             % Store properties of plot in variable
set(sp,'LineStyle','--')         % and modify these
box on
grid on

% Grid plot
figure('Name','Grid-Plot')
mesh(xx,yy,zz)
box on
grid on

% 2D contour plot
figure('Name','Contour-Plot')
contour(xx,yy,zz)
box on
grid on

%- 2.2 User defined functions -%
% Functions can be defined by the following notation:
% function [outputs] = function-name(inputs)
% The function can be called by, e.g.
derivatives = LTI_system(eye(2),[0;1],[2;2],1);

% The declaration of functions (within a script) has to appear at
% the bottom of the script
function [dx] = LTI_system(A,B,x,u)
    % DGL of a LTI system
    dx = A*x + B*u;
end