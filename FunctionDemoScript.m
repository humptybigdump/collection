% This script should illustrate how you can use functions to 'store away'
% calculations and procedures, and in this way structure and simplify your
% program code. We will call the demoFunction, which we defined in another
% file called demoFunction.m

%% ---
% Let us first see what can be done with the function that we defined

help demoFunction

%% ---
% We see from the description that the function can produce two outputs.
% When we simply call the function, it only produces one output, though!

demoFunction(3,5)

% The way to store multiple outputs from one function is to provide an
% array with several variables in it:

[sum_output,product_output] = demoFunction(3,5)

% In this manner, we store the outputs of the function into new variables,
% which we can from there on use for further calculations.

%% ---
% We can not only enter numbers directly into functions, we can also use
% variables as inputs to functions.

input_one = 4;
input_two = 7;

[sum_output,product_output] = demoFunction(input_one,input_two);

% The following two commands are only to display the values assigned to the
% output variables.
sum_output
product_output