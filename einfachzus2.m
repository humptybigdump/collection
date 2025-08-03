clf
t=0:0.01:2*pi;
x=cos(t); y=sin(t);
plot(x,y,'r');
hold on;
plot(0,0.5,'ok')
axis([-1 1 -1 1]); 
axis equal;
pause(3);
for i=0:0.01:1;
plot((1-i)*x,(1-i)*y+1/2*i,'r');
hold on;
plot(0,0.5,'ok')
axis([-1 1 -1 1]);
axis equal;
hold off;
pause(0.02);
end;