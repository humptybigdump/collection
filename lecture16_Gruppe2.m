clear all
close all

%% Gruppe 2
%% a)
syms x a b n;

% f=(x^3);
fun = @(x) x.^4-x*2;

trans=(1/2)*(x+1);

n1= 2;

n2_1= (1/2)*((1/sqrt(3))+1)*1;
n2_2= (1/2)*(-(1/sqrt(3))+1)*1;

n3_1= (1/2)*(0+1)*(8/9);
n3_2= (1/2)*((sqrt((3/5))+1))*(5/9);
n3_3= (1/2)*(-(sqrt((3/5))+1))*(5/9);

I_n1=2;
I_n2=n2_1+n2_2;
I_n3=n3_1+n3_2+n3_3;


a=0;
b=1;

e1 =abs( I_n1- integral(fun,a,b));
e2 =abs( I_n2- integral(fun,a,b));
e3 =abs( I_n3- integral(fun,a,b));

%% Romberg Integration

T_00=0.75;
T_01=0.708333333;
T_02=0.69702380952;

T_11= (4*T_01-T_00) / 3 ;
T_12= (4*T_02-T_01) / 3 ;
T_22= (16*T_12-T_11) / 15 ;









