clf
t=0:0.01:2*pi;
x=cos(t); y=sin(t);
plot(x,y,'r');
axis([-1 1 -1 1]); 
axis equal;
pause(3);
for i=0:0.01:1;
plot((1-i)*x,(1-i)*y,'r');
hold on;
axis([-1 1 -1 1]);
axis equal;
hold off;
pause(0.02);
end;
plot(0,0,'*b');
