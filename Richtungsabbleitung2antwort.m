clf

r = 1.2;
h = 1.2;
m = h/r;
[R,A] = meshgrid(linspace(0,r,101),linspace(0,2*pi,201));
X = R .* cos(A);
Y = R .* sin(A);
Z = m^2*R.^2;
mesh(X,Y,Z)
hold on;
[X,Z]=meshgrid(-0.5:0.01:1.3,-0.5:0.01:1.3);
Y=1-X;
b=0*Z+2;
mesh(X,Y,Z,b)
hold on
[X,Z]=meshgrid(-0.5:0.01:1.3,-0.5:0.01:1.3);
Y=X;
c=0*Z+2.5;
mesh(X,Y,Z,c)
hold on
axis equal
axis([-2 2 -2 2 0 3])
hold on;
plot3(1/2,1/2,1/2,'*g');
plot3([0.5 1.5], [0.5, 1.5], [0 0], 'r');
plot3([1.5 1.51], [1.5 1.51], [0 0], '>r');
plot3([0.5 1.5], [0.5, -0.5], [0 0], 'b');
plot3([1.5 1.51], [-0.5 -0.51], [0 0], '>b');
plot3([0.5 1.5], [0.5, 1.5], [0.5 2.5], 'r');
plot3([0.5 1.5], [0.5, -0.5], [0.5 0.5], 'b');

