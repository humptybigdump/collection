clf

% Cone around the z-axis, point at the origin
[X,Z]=meshgrid(-1:0.01:1,0:0.01:4);
Y=1-X.^2;
mesh(X,Y,Z)
%hold on;
%[X,Z]=meshgrid(-1:0.005:1,-1:0.005:1);
%Y=0*X+1/2;
%b=Y-2;
%mesh(X,Y,Z,b);
%hold on
%axis equal
%axis([-2 2 -2 2 -1 3])
%hold on;
%plot3(1/4,1/4,1/8,'*');
%phi = -pi/3;
%X1 = X*cos(phi) - Z*sin(phi);
%Y1 = Y;
%Z1 = X*sin(phi) + Z*cos(phi);
% Previous cone, rotated by angle phi about the y-axis
%mesh(X1,Y1,Z1)

%theta = pi/4;
%X2 = X1*cos(theta) - Y1*sin(theta);
%Y2 = X1*sin(theta) + Y1*cos(theta);
%Z2 = Z1;
% Second cone rotated by angle theta about the z-axis
%mesh(X2,Y2,Z2)

%xlabel('x')
%ylabel('y')
%zlabel('z')