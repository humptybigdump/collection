clf
t=0:0.01:2*pi;
x=-1:0.01:1;
y=x.^2;
plot(x,y,'r');
hold on;
z=2*x.^2-1;
plot(x,z,'r');
plot([0 0],[0.5 1],'k')
plot(0,0.5,'.k')
axis([-1 1 -1 1]); 
axis equal;
pause(3);
for i=0:0.01:1;
plot((1-i)*x,(1-i)*y,'r');
hold on;
plot([0 0],[0.5 1],'k')
plot((1-i)*x,(1-i)*z,'r');
plot(0,0.5,'.k')
axis([-1 1 -1 1]);
axis equal;
hold off;
pause(0.02);
end;
plot([0 0],[0.5 1],'k');
hold on;
plot(0,0.5,'.k')
plot(0,0,'*b');
axis([-1 1 -1 1]);
axis equal;
