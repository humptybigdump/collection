clc; clear; close all;


A = @(phi) [cosd(phi) -sind(phi) 0 ;
     sind(phi)  cosd(phi) 0 ;
      0        0        1];

[V,D] = eig(A(45))


u   = [0;0;1];
U   = [0    -u(3)  u(2);
       u(3)  0    -u(1);
       -u(2) u(1)  0];

A(45)
R = @(phi) cosd(phi)*eye(3,3) + (1-cosd(phi))*(u*u.') + sind(phi)*U;
R(-45)
