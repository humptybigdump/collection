function [xnew,ynew] = general_method(x,y,tau,w,version)
% perform one time step of one of the six methods from the lecture
% applied to the FPUT problem in the one step formulation.

% parameter 'version' determines which method to be used:
% 1: Gautschi
% 2: Deuflhard
% 3: Garcia-Archilla
% 4: Hochbruck Lubich
% 5: Hairer Lubich
% 6: Hochbruck Grimm

xnew =   [ones(3,1); cos(tau*w)*ones(3,1)]    .* x + tau * [ones(3,1); mysinc(tau*w)*ones(3,1)] .* y;
ynew = - [zeros(3,1); w*sin(tau*w)*ones(3,1)] .* x +       [ones(3,1); cos(tau*w)*ones(3,1)]    .* y ;
switch version
    case 1 % Gautschi
        psi = mysinc(tau*w/2)^2;
        phi = 1;
    case 2 % Deuflhard
        psi = mysinc(tau*w);
        phi = 1;
    case 3 % Garcia-Archilla
        psi = mysinc(tau*w)^2;
        phi = mysinc(tau*w);
    case 4 % Hochbruck Lubich
        psi = mysinc(tau*w/2)^2;
        phi = mysinc(tau*w) * (1+1/3*sin(tau*w/2)^2) ;
    case 5 % Hairer Lubich
        psi = mysinc(tau*w)^2;
        phi = 1;
    case 6 % Hochbruck Grimm
        psi = mysinc(tau*w)^3;
        phi = mysinc(tau*w);
end
xnew = xnew + tau^2/2 * [ones(3,1); psi*ones(3,1)] .* g( [ones(3,1); phi*ones(3,1)] .* x );

ynew = ynew + tau/2 * (   [ones(3,1); cos(tau*w)/mysinc(tau*w)*psi*ones(3,1)] .* g( [ones(3,1); phi*ones(3,1)] .* x )...
                        + [ones(3,1); 1/mysinc(tau*w)*psi*ones(3,1)]          .* g( [ones(3,1); phi*ones(3,1)] .* xnew ) );

end