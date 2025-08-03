%order of operation matters!!
clear all 
close all
clc
%
format longe
%
a=0.1; b=0.2; c=0.3;
r1=a+b-c;
r2=a-c+b;
r1 == r2
if(r1==r2)
    disp('r1=r2')
else
    disp('r1 differs from r2 by')
    r2-r1
end
