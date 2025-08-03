function bogenlaenge

t=0:0.01:4*pi;
v=2*sin(t);
w=cos(t);
plot3(v,w,t);
pause(4);
for i=1:10,
    theta=0:pi/(2*i):4*pi;
    x=2*sin(theta);
    y=cos(theta);
    plot3(v,w,t,'b');
    hold on;
    plot3(x,y,theta,'r');
    hold off;
    pause(1.5);
end;
for i=1:10,
    theta=0:pi/(2*i):4*pi;
    x=2*sin(theta);
    y=cos(theta);
    plot3(x,y,theta,'r');
    pause(1.5);
end;