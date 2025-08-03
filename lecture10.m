% Group 2

clear all;
close all;

syms x

q1 = 1/sqrt(2);
q2 = sqrt(3/2)*x;
q3 = 3/2*sqrt(5/2)*(x^2-1/3);

fun1 = @(x) x.^4 .*1/sqrt(2);
fun2 = @(x) x.^4 .*sqrt(3/2).*x;
fun3 = @(x) x.^4 .*3/2.*sqrt(5/2).*(x.^2-1/3);

gamma0 = integral(fun1,0,1);
gamma1 = integral(fun2,0,1);
gamma2 = integral(fun3,0,1);

phi_dach = gamma0.*q1 + gamma1.*q2 + gamma2.*q3;

f = x.^4;


figure
fplot(f,[0 1],'b');
hold on
fplot(phi_dach,[0 1],'r');
hold off
grid on
legend('x^4','L2-Approximation','Location', 'northwest');