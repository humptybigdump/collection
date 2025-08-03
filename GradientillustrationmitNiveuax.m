clf

[R,A] = meshgrid(linspace(0,1,301),linspace(0,2*pi,601));
X = R .* cos(A);
Y = R .* sin(A);
Z = 2-R.^2;
mesh(X,Y,Z)
hold on;
plot3(0.5*ones(1,16), 0.5*ones(1,16), 0: 0.1: 1.5,':k' );
plot3(1/2,1/2,3/2,'*k');
plot3([0.5 1.5], [0.5, 1.5], [0 0], 'r');
plot3([1.5 1.51], [1.5 1.51], [0 0], '>r');
plot3([0.5 -0.5], [0.5, 1.5], [0 0], 'b');
plot3([-0.5 -0.51], [1.5 1.51], [0 0], '<b')
plot3([0.5 1.5], [0.5, -0.5], [0 0], 'k');
plot3([1.5 1.51], [-0.5 -0.51],  [0 0], '>k')
plot3([0.5 -0.5], [0.5, -0.5], [0 0], 'm');
plot3( [-0.5 -0.51], [-0.5 -0.51],  [0 0], '<m')
t=0:0.01:2*pi;
plot3(sqrt(2)*cos(t)/2, sqrt(2)*sin(t)/2,0*sin(t),'g');
plot3(sqrt(2)*cos(t)/5, sqrt(2)*sin(t)/5,0*sin(t),'r');
plot3(cos(t), sin(t),0*sin(t),'b');
%hold on
%axis equal
%axis([-2 2 -2 2 -1 3])
%hold on;
%plot3(1/2,1/2,1/2,'*g');