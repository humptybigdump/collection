%% ========================= FOR LOOP - SIMPLE ======================== %%
clear all; clc; 
A = [0:1:100];

% Loop over all
for ii = 1:length(A)
    disp(A(ii))
    pause(0.05)
end

% Steps in the iterations
for ii = 1:10:length(A)
   disp(A(ii)) 
   pause(0.05) 
end

% Overwrite the output by not using an index
for ii = 1:10:length(A)
    B = A(ii) - 1;
end

index = 1;
for ii = 1:10:length(A)
    B(index) = A(ii) - 1;
    index = index + 1;
end

% ---------- MINI ASSIGNMENT
clear all; clc;
A = 70:0.5:80;
for ii = 1:length(A)
   B(ii) = A(ii) * 2; 
end

%% ===================== NESTED LOOPS ===================== %%
clear all; clc; 
A = zeros(3,4);
n = 1;

% Fill a matrix with numbers increasing by 1
for ii = 1:1:3
    for kk = 1:1:4
        A(ii,kk) = n;
        n = n + 1;
    end
end

% Linearize the matrix
B = zeros(1,12);
m = 1;
for ii = 1:1:3
    for kk = 1:1:4
        B(1,m) = A(ii,kk);
        m = m + 1;
    end
end