% -----
% Exercise 1.4: subtractive cancellation
% - happens when subtracting very similar numbers.
% Compute : 
% (a + small) - a
% (a + small) + a
% try 
% a = 100
% small = 1e-5
% -----
clear all 
close all
clc
format longe   % sets the output format in the command window
               % for more info just type >> help format 
%-------------------------
a = 100;
small = 1e-5;
sub=(a + small) - a  
add=(a + small) + a  % notice : it does not affect addition 

% Recall: 
% when subtracting very similar numbers
% it might result in a number with a smaller exponent
% and thus a loss in significant digits
% -> one way to mitigate this is by using higher precisions.



