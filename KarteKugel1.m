clf

[theta,phi] = meshgrid(linspace(0,pi,201),linspace(0,2*pi,601));
X = sin(theta) .* cos(phi);
Y = sin(theta) .* sin(phi);
Z = cos(theta);
a=X*0;
for s=0:-0.1:-pi,
subplot(2,1,1)
fill([0 0 2*pi 2*pi],[0 -pi -pi 0],'g')
axis([0 2*pi -pi 0])
hold on;
plot(0*(0:-0.1:s)+pi/2,0:-0.1:s,'*b');
plot(0*(0:-0.1:s)+pi/4,0:-0.1:s,'*b');
hold on;
axis equal;
subplot(2,1,2)
mesh(X,Y,Z,a+2)
axis([-1.5 1.5 -1.5 1.5 -1.5 1.5])
hold on;
plot3(sin(s) .* cos(pi/2),sin(s) .* sin(pi/2),cos(s) ,'*b');
plot3(sin(s) .* cos(pi/4),sin(s) .* sin(pi/4),cos(s) ,'*b');
hold on;
axis equal;
pause(0.05)
end;
