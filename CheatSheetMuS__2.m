% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %  
% % % % % % % MATLAB CHEAT SHEeT - MODELING AND SIMULATION % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 


% Small variables like x and y will be either row or column vectors and A
% will always be a matrix. 

% % % % % % % % % % % % % % %  Basic Commands % % % % % % % % % % % % % % % 

clc                        % Clear command window
clear all                  % Clear all variables
%close all                  % Close all plots
%clf                        % Clear all plots
a = 5 ;                    % Semicolon suppreses output
whos                       % List all variables defined
disp('text')               % Print text


% % % % % % % % % % % % % % Manipulation of Variables % % % % % % % % % % %


a = 500                    % Define variable a to be 500
x = [3, 1, 4]              % Set x to be a row vector
y = [3; 1; 4]              % Set x to be a column vector
A = [3, 1, 4, 1;           % Set A to be a 3x4 matrix
     5, 9, 2, 6;
     5, 3, 5, 8]
x(2) = 7                   % Change x from [3,1,4] to [3,7,4]
A(2,1) = 0                 % Change A21 from 5 to 0


% % % % % % % % % % %  Basic Arithmetic and Functions % % % % % % % % % % %


1*2, 3+4, 5-6, 7/8         % Mutiply, add, subtract and divide
2^8                        % Compute 2^8
sqrt(16)                   % Compute square root 16
log(5)                     % Compute ln(5)
log10(100)                 % Compute log(100)
abs(-10)                   % Compute |-10|
sin(2*pi/6)                % Compute sine from 2*pi/6
ceil(3.8)                  % Outputs 4.0
floor(3.8)                 % Outputs 3.0



% % % % % % % % % % % Construct Matrices and Vectors % % % % % % % % % % % 


zeros(5, 5)                % Create a 5 x 5 matrix of zeros (Pre-Allocation)
ones(6, 7)                 % Create a 6 x 7 matrix of ones
eye(3)                     % Create a 3 x 3 identity matrix
eye(9, 10)                 % Make a 9 x 10 identity matrix
linspace(0,1,1000)         % Generates 1000 points between 0 and 1
logspace(0,1,1000)         % Creates a vector with 1000 elements where the 
                           % log of the spacing is evenly increasing
                           % between 0 and 1
1:99                       % Row vector of 1,2,...,88,99




% % % % % % % % % % % Entries of Matrices and Vectors % % % % % % % % % % %



abs(x)                     % The absolute value of x
eps                        % Floating point accuracy
1e6                        % 10^6
sum(x)                     % Sums elements in x
round(10.1)                % Rounds to the nearest integer
fix(10.6)                  % Rounds to the nearest integer toward zero
                           


% % % % % % % % % % % % % % % % Cell Manipulation % % % % % % % % % % % % %

z = cell(5,4)              % 5 x 4 cell array
z{1,2}                     % Access cell element 1,2
cell2mat(z)                % Transform cell to matrix


% % % % % % % % % % % % Operations on Matrices and Vectors % % % % % % % % 

x = [1; 2; 3] ;
y = [3; 4; 5] ;
A = [1, 2, 3;
     4, 5, 6;
     7, 8, 9];
B = [10, 11, 12;
     13, 14, 15;
     16, 17, 18];
 
x + 5                      % Add 5 to every element of x
x + y                      % Elementwise addition of two vectors x and y
10*x                       % Multiply every element of x by 10
A*y                        % Product of two matrices
A.*B                       % Element-wise product of two matrices(important
                           % if matrices are not quadratic)
A^4                        % Square matrix A to the fourth power
A.^4                       % Every element of A to the fourth power
cos(A)                     % Compute the cosine of every element of A
abs(A)                     % Compute the absolute values of every element of A
A'                         % Transpose of A
det(A)                     % Compute the determinant of A
size(A)                    % Get the size of A
                           

%%%%%%%%% Entries of Matrices and Vectors 5%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x = linspace(0,8,9);       % Vector [1 2 3 4 5 6 7 8 9]
A = eye(5)                 % 5 x 5 unit matrix

x(5:8)                     % The 5th to the 8th element of 8
x(5:end)                   % The 5th to the last element of x
x(1:2:end)                 % Every second element of x from the first to the last

A(3,:)                     % Get the third row of A
A(:,5)                     % Get the 5th column of A
A(5,2:5)                   % Get the first to fifth elements of the 5th row

%%%%%%%%%%%%%%%%%%%%%%%%%% Constants %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi                         % pi = 3.14159.......
inf                        % Infinity
NaN                        % Not a Number (i.e 0/0)

%%%%%%%%%%%%%%%%%%%%%%%% Debugging %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


tic                        % Starts timer
toc                        % Stops timer




%%%%%%%%%%%%%%%%%%%% Solving linear equations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A = [1,3,5 ;
     7,5,3 ;
     9,2,1 ];

 b = [1;3;5];
 

inv(A)                     % Compute the inverse A^-1
eig(A)                     % Compute the eigenvalues of A
[L,U,P] = lu(A)            % The LU factorization PA = LU
[V,D]=eig(A)               % V are the eigenvectors of A and the diagonals diag(D) are the eigenvalues of A
A\b                        % Compute the solution x to Ax = b

%%%%%%%%%%%%%%%%%%%%%%%%%% Logicals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a = 10                     % assign a the value of 10

a == 5                     % Test if a equals to 5 (false = 0)
a == 10                    % Test if a equals to 10(true = 1)
a >= 5                     % Test if a greater than or equals to 5 (true = 1)
a < 9                      % Test if a smaller than 9 (false = 0)
a ~= 4                     % Test if a is not equal to 4(true = 1)
a > 1 && a ~= 10           % Test if a is greater than 1 AND not equal to 10(false = 0)
a > 1 || a ~= 5            % Test if a is greater than 1 OR not equal to 10(true = 1)


%%%%%%%%%%%%%%%%%%%%%%%% For loops %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for k = 1:10
    disp(k);               % Display current value of k
end

%%%%%%%%%%%%%%%%%%%%%%%%% Conditional Statements %%%%%%%%%%%%%%%%%%%%%%%%%%

if a > 90
    disp('a greater than 90');
elseif a == 90
    disp('a equals to 90');
else
    disp('none of the conditions is met');
end
%%%%%%%%%%%%%%%%%%%%%%%% While Loops %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

k = 0;
while k < 5
    k = k + 1;
end


%%%%%%%%%%%%%%%%%%%% Plotting & Subplot %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x = linspace(-5*pi,5*pi,1000);
y1 = sin(x);
y2 = cos(x);

figure();                          % Open first plot
plot(x,y1,'g-','LineWidth',3);     % Plot green sin(x)
hold on;                           % Adding additional curve
plot(x,y2,'r-','LineWidth',3);     % Plot red cos(x)
grid on;
set(gca,'fontsize',20);    

axis([-5*pi, 5*pi, -1.5, 1.5]);    % Set the axis limits

xlabel('x','FontSize',20);         % Add axis label
ylabel('y','FontSize',20);

title('A plot of sin(x) and cos(x)','FontSize',20);  % Add a title
legend('sin(x)','cos(x)');

figure();                          % Open second plot
% code for subplots

x = linspace(0,10,50);
y = rand(50,1);

subplot(2,2,1), plot(x,sin(x),'Color','red','LineWidth',3)
set(gca,'fontsize',14)
axis([0,2*pi,-1,1]), axis square

subplot(2,2,2), plot(x,cos(x),'Color','blue','LineWidth',3)
set(gca,'fontsize',14)
axis([0,2*pi,-1,1]), axis square

subplot(2,2,3:4), plot(x,y,'LineWidth',3)
set(gca,'fontsize',14)
                


