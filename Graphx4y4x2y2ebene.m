clf

[X,Y]=meshgrid(-2.01:0.02:2,-2.01:0.02:2);
Z=(X.^4+Y.^4)./[(X.^2+Y.^2)]+X;
mesh(X,Y,Z);
hold on;
Z1=X;
c=0*X-2;
c=0*X-2;
mesh(X,Y,Z1,c)
plot3([0], [0], [0],'r*')
