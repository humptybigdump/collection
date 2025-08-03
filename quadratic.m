function [x1, x2] = quadratic(a,b,c)
% This function returns the roots of a quadratc equation.
% It takes three input terms. The coefficient of x^2, x and the constant

% Calculation of the discriminant
d = disc(a,b,c);
% Calculatuion of the root
x1 = (-b + d) / (2*a);
x2 = (-b - d) / (2*a);

end

% Subfunction
function dis = disc(a,b,c)
% Returns the discriminant
% Takes the two coefficient and constant as well

% Calculation of the discriminant
dis = sqrt(b^2 - 4*a*c);

end
