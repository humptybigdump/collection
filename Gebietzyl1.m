clf

[R,A] = meshgrid(linspace(0,2,301),linspace(0,2*pi,601));
X = R .* cos(A);
Y = R .* sin(A);
Z1 = 2*Y+5;
a=X*0;
mesh(X,Y,Z1,a)
hold on;
Z2 = X.^2+(Y+1).^2;
mesh(X,Y,Z2,a+1)
mesh(X,Y,a,a+2)

