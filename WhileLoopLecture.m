%% ===================== WHILE LOOPS ===================== %%
clear all; clc;
A = 1;
% While this statement remains true, the following computation will be done
while A < 10
    fprintf('The variable A has number %.0f at the moment\n', A);
    A = A + 1;
end

%% ===================== Mini assignment while loop ==================== %%
clear all; clc;

x = 50;
while x > 20
   x = x - (x*0.75);
   pause(2)
end
disp('X is now 20')