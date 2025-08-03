function [q,q_prime,q_dprime] = Weierstrass
%% Description: This function realizes the so-called Weierstrass function.
% Input:
% Output: q ... the Weierstrass function we want to derive
%         q_prime ... the derivative of the Weierstrass function
%         q_dprime ... the 2nd derivative of the Weierstrass function
%
% Further information: To get the function handle use
%               [f,f_prime,f_dprime] = Weierstrass;
% Example:
% [f,f_prime,f_dprime] = Weierstrass;
% t = linspace(-1,1,500);
% plot(t,f(t),t,f_prime(t),t,f_dprime(t),'LineWidth',2.0);
% legend('$f$','$f^\prime$','$f^{\prime \prime}$','Interpreter','LaTex','FontSize',18);

q = @(x) 0;
q_prime = @(x) 0;
q_dprime = @(x) 0;
a = 9/10;
b = 3;
for m = 1 : 20
    q = @(x) q(x) - a^m * 1/(b^m*pi)^2 * cos(b^m*pi*x); 
    q_prime = @(x) q_prime(x) + a^m * 1/(b^m*pi) * sin(b^m*pi*x);
    q_dprime = @(x) q_dprime(x) + a^m .* cos(b^m*pi*x);
end


end

