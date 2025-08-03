% Subtractive Cancellation, another example 
% - happens when subtracting very similar numbers.
% - one way to mitigate is by using higher precisions.
clear all
close all
clc
%
format longe %just affects the display, 
%
%Example 3.8, from Chapra:
%Compute the roots of an quadratic equation with
%a =1, b=3000.001, c=3.
%Roots are :
%x1 = (-b + sqrt(b.^2 -  4*a*c)) /(2*a); Notice this is actually subtraction
%x2 = (-b - sqrt(b.^2 -  4*a*c)) /(2*a); While this is summation
%The exact solutions are x1=-0.001 and x2=-3000.
%
%when using single precision
a=single(1); b=single(3000.001); c=single(3);
d=sqrt(b.^2-4*a*c)
x1=(-b+d)/(2*a)
x2=(-b-d)/(2*a)
%
%when using double precision
aa=double(1); bb=double(3000.001); cc=double(3);
dd=sqrt(bb*bb-4*aa*cc)
xx1=(-bb+dd)/(2*aa)
xx2=(-bb-dd)/(2*aa)
%
%Error in x1 for single and double precision
E_x1=abs(-0.001-x1)
E_xx1=abs(-0.001-xx1)
%Error in x2 for single and double precision
E_x2=abs(-3000-x2)
E_xx2=abs(-3000-xx2)
%Notice : 
%Summation of very similar numbers works fine.
%Subtraction of very similar numbers can lead to huge errors.
%The error in x1 is much larger than in x2.



