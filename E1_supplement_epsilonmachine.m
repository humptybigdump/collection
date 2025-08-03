% a routine to find the machine epsilon 
% i.e. find the smallest positive number 
% that when added to unity gives a result
% that differs from unity.
%
clear all
close all
clc

%-------------------
x=1;
epsilon=double(1); % Define 1.0 with double precision
%epsilon=single(1); % Define 1.0 with single precision
while(x < x + epsilon)
  epsilon=epsilon*0.5;
end
epsilon=epsilon*2

% %----- another algorithm
% x=double(1);
% for i=0:60
%     epsilon=2^(-i);
%     if x==x+2^(-i)
%         epsilon=2^-(i-1);
%         return
%     end
% end

% the built-in matlab expression which
% gives the machine accuracy is 
%eps(double(x))
%eps(single(x))

