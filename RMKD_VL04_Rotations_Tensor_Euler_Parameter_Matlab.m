clc
clear
close all

%% Rotation Tensor and Euler Parameters

% Original Vector and Axis-Angle Rotation
r   = [1;2;-0.3];
phi = 78;
u   = [0.2;1;1];
u   = u/norm(u);

% Rotation Tensor from Axis-Angle
R = cosd(phi)*eye(3,3)+(1-cosd(phi))*(u*u.')+sind(phi)*CrossMat(u);


% Rotation Tensor from Euler Parameters
q0 = cosd(phi/2);
q  = sind(phi/2)*u;
Rq = (2*q0^2-1)*eye(3,3)+2*(q*q.')+2*q0*CrossMat(q);

% Vergleich Axis-Angle vs. Euler Parameter
R-Rq

% Axis-Angle from Rotation Tensor (Sheppert)
% Can later be put into a function
qs      = zeros(4,1);
[Rii,i] = max([trace(R),R(1,1),R(2,2),R(3,3)]);
qs(i)   = 1/2*sqrt(1+2*Rii-trace(R));

switch i
    case 1
        qs(2) = 1/4*(R(3,2)-R(2,3))/qs(1);  % Eq. (7)
        qs(3) = 1/4*(R(1,3)-R(3,1))/qs(1);  % Eq. (6)
        qs(4) = 1/4*(R(2,1)-R(1,2))/qs(1);  % Eq. (5)
    case 2
        qs(1) = 1/4*(R(3,2)-R(2,3))/qs(2);  % Eq. (7)
        qs(3) = 1/4*(R(2,1)-R(1,2))/qs(2);  % Eq. (10) 
        qs(4) = 1/4*(R(1,3)-R(3,1))/qs(2);  % Eq. (9)
    case 3
        qs(1) = 1/4*(R(1,3)-R(3,1))/qs(3);  % Eq. (6)
        qs(2) = 1/4*(R(2,1)-R(1,2))/qs(3);  % Eq. (10) 
        qs(4) = 1/4*(R(3,2)-R(2,3))/qs(3);  % Eq. (8)
    case 4
        qs(1) = 1/4*(R(2,1)-R(1,2))/qs(4);  % Eq. (5)
        qs(2) = 1/4*(R(1,3)-R(3,1))/qs(4);  % Eq. (9) 
        qs(3) = 1/4*(R(3,2)-R(2,3))/qs(4);  % Eq. (8)
end


if norm(qs(2:4))>0
 us   = qs(2:4)/norm(qs(2:4));
 nq   = dot(qs(2:4),us);        % q*u = sin(phi/2)
 phis = 2*atan2(nq,q0)*180/pi;    % phi = 2*arctan(sin(phi/2)/cos(phi/2))
else
 phis = 0;
 us = [nan;nan;nan];
end

% Vergleich Rückwärtsoperation vs. Axis-Angle
phis-phi
us-u

% Rotation mit Quaternionen
qq = qs;
rq = [0; r];

sq = quatprod(qq,quatprod(rq,invq(qq)));

s = R*r;
s-sq(2:4)




%% Functions
function U = CrossMat(u)
 U = [ 0    -u(3)  u(2);
       u(3)  0    -u(1);
      -u(2)  u(1)  0   ];
end

% Inverse quaternion
function q1 = invq(q)
 q1 = [q(1);-q(2:4)];
end

% Quaternionenprodukt z=x*y
function z = quatprod(p,q)  % https://de.wikipedia.org/wiki/Quaternion
 z = [p(1)*q(1) - p(2)*q(2) - p(3)*q(3) - p(4)*q(4); 
      p(1)*q(2) + p(2)*q(1) + p(3)*q(4) - p(4)*q(3); 
      p(1)*q(3) - p(2)*q(4) + p(3)*q(1) + p(4)*q(2); 
      p(1)*q(4) + p(2)*q(3) - p(3)*q(2) + p(4)*q(1)];
end