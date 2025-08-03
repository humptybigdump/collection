clf
for phi2=-pi/2:pi/6:pi/2,
for theta2=0:pi/180:pi/6, 
[theta,phi] = meshgrid(linspace(0,pi/6,201),linspace(-pi/2,pi/2,601));
X = sin(theta) .* cos(phi);
Y = sin(theta) .* sin(phi);
Z = cos(theta);
a=X*0;
mesh(X,Y,Z,a)
axis([-1.5 1.5 -1.5 1.5 -1.5 1.5])
axis equal;
hold on;
X1 = sqrt(2)*sin(theta) .* cos(phi);
Y1 = sqrt(2)*sin(theta) .* sin(phi);
Z1 = sqrt(2)*cos(theta);
mesh(X1,Y1,Z1,a+0.5)
[r,phi1] = meshgrid(linspace(0,sqrt(2),101),linspace(-pi/2,pi/2,601));
X2 = r* sin(pi/6) .* cos(phi1);
Y2 = r* sin(pi/6) .* sin(phi1);
Z2 = r* cos(pi/6);
mesh(X2,Y2,Z2,X2*0+1+0.5*(r>1))
plot3(0*t,0*t,t,'*r');
t=0:0.01:1.5; 
plot3(sin(theta2) .* cos(phi2)*t,sin(theta2) .* sin(phi2)*t,cos(theta2)*t,'*r');
hold off;
pause(0.001);
end;
    end;