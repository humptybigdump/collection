function [res] = g(x)
% If U(x) is the nonlinear part of the Hamiltonian of the FPUT-problem (see
% Eq. 5.2 in Sect I.5.1 in GNI) for m=3, then g evaluates -grad(U(x)).
%
% x = ( x_{0,1}, x_{0,2}, x_{0,3}, x_{1,1}, x_{1,2}, x_{1,3} )^T
% displacements and expansions of the stiff springs

% axuiliary variables
y1 = (x(1)-x(4))^3;             % (x_{0,1} - x_{1,1})^3
y2 = (x(2)-x(5)-x(1)-x(4))^3;   % (x_{0,2} - x_{1,2} - x_{0,1} - x_{1,1})^3
y3 = (x(3)-x(6)-x(2)-x(5))^3;   % (x_{0,3} - x_{1,3} - x_{0,2} - x_{1,2})^3
y4 = (x(3)+x(6))^3;             % (x_{0,3} - x_{1,3})^3

res = - [ y1 - y2;
          y2 - y3;
          y3 + y4;
         -y1 - y2;
         -y2 - y3;
         -y3 + y4 ];

end