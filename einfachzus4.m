clf
t=0:0.01:2*pi;
x=cos(t); y=sin(t);z=0*t;
plot3(x,y,z,'r');
hold on;
plot3(0,0,0,'ok')
axis([-1 1 -1 1 -1 1]); 
pause(3);
for i=0:0.01:1;
axis([-1 1 -1 1 -1 1]);
plot3((1-i)*x,(1-i)*y,z-i,'r');
hold on;
plot3(0,0,0,'ok')
pause(0.02);
end;
plot3(0,0,-1,'*b');