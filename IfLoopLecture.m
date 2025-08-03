%% ======================== IF LOOP - NUMBER 1 ======================== %%
clear all; clc; 
% Variables
x           = 10;
threshold   = 20.5;
% If loop to check the value of x
if x >= threshold
    fprintf('x is equal or greater than %f!', threshold)
else
   fprintf('x is smaller than %f!', threshold)
end

%% ======================== IF LOOP - NUMBER 2 ======================== %%
clear all; clc; 
% Variables
x               = 2;
threshold_1     = 15;
threshold_2     = 30;

if x > threshold_1 && x < threshold_2
    
    fprintf('x is greater than the first threshold of %.1f', threshold_1);

elseif  x > threshold_2
    
    fprintf('x is greater than the second threshold of %.1f', threshold_2);
    
else 
    
    fprintf('x is smaller than the first threshold of %.0f', threshold_1);
    
end

%% ======================== IF LOOP - NUMBER 3 ======================== %%
clear all; clc; 
% Variables
whoami = 'student';

if strcmp(whoami, 'student') 
   
    text_1 = 'I am a MATLAB student';
    
elseif strcmp(whoami, 'teacher')
    
    text_1 = 'I am a MATLAB teacher';

else
    
    text_1 = 'I don''t know who I am';
    
end

fprintf('%s\n', text_1)

%% ======================== IF LOOP - NUMBER 3 ======================== %%
clear all; clc; 
% Variables
x = 5;

if x == 5
    Var1 = x + x;
elseif x < 5
    Var2 = x * x;
else
    Var3 = x / 2;
end