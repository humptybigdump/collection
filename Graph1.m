clf

[X,Y]=meshgrid(-8:0.05:8,-8:0.05:8);
Z=1/8*(Y.^2-X.^2);
mesh(X,Y,Z)
hold on;
[Y,Z]=meshgrid(-8:0.05:8,-8:0.05:8);
X=0*Y;
b=X+8;
mesh(X,Y,Z,b);

