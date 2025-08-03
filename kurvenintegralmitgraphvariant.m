clf

r = 1.5;
h = 1.5;
m = h/r;
[R,A] = meshgrid(linspace(0,r,101),linspace(0,pi/2,201));
X1 = cos(A) ;
Y1 = sin(A);
Z1 = R.* sqrt(A);
h1=surf(X1,Y1,Z1)
set(h1,'LineStyle','none')
colormap([0  0.95  0])
hold on;
[R,A] = meshgrid(linspace(r,0,101),linspace(pi/2,pi,201));
X2 = cos(A) ;
Y2 = 2-sin(A);
Z2 = R.* sqrt(A);
h2=surf(X2,Y2,Z2)
set(h2,'LineStyle','none')
colormap([0  1  1])
axis([-1 1 0 2 0 3])
hold on;
%[X3,A] = meshgrid(linspace(-1,1,101),linspace(0,pi,201));
%Y3=sin(A).*(A<pi/2)+(2-sin(A)).*(A>=pi/2);
%Z3=r*sqrt(acos(X3)+(Y3-sin(A)).^2.*(A<pi/2)+(Y3-2+sin(A)).^2.*(A>=pi/2));
%h3=surf(X3,Y3,Z3);
[X3,Y3] = meshgrid(linspace(-1,1,101),linspace(0,2,101));
%Z3=r*sqrt(acos(X3)+(Y3-sqrt(1-X3.^2)).^2.*(Y3<1)+(Y3-2+sqrt(1-X3.^2)).^2.*(Y3>=1));
Z3=r*sqrt(acos(X3)+(Y3-sqrt(1-X3.^2)).^2.*(X3>0)+(Y3-2+sqrt(1-X3.^2)).^2.*(X3<=0));
h3=surf(X3,Y3,Z3);
t=0:0.01:pi;
x=cos(t);
y=sin(t).*(t<pi/2)+(2-sin(t)).*(t>=pi/2);
z=0*t; plot3(x,y,z,'*r'); 
