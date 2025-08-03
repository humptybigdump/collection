clf

r = 1.5;
h = 1.5;
m = h/r;
[R,A] = meshgrid(linspace(0,r,101),linspace(0,pi/2,201));
X1 = cos(A) ;
Y1 = sin(A);
Z1 = R.* sqrt(A);
% Cone around the z-axis, point at the origin
mesh(X1,Y1,Z1)
hold on;
[R,A] = meshgrid(linspace(r,0,101),linspace(pi/2,pi,201));
X2 = cos(A) ;
Y2 = 2-sin(A);
Z2 = R.* sqrt(A);
mesh(X2,Y2,Z2)
% axis equal
axis([-1 1 0 2 0 3])
%hold on;
hold off;
pause(5);
for i=1:8, 
%            [R,A] = meshgrid(linspace(0,r,101),linspace(0,pi/2,2*i));
            for theta=0:pi/(4*i): (pi/2-pi/(4*i)),
                fill3([cos(theta), cos(theta+pi/(4*i)),cos(theta+pi/(4*i)), cos(theta)], [sin(theta), sin(theta+pi/(4*i)),sin(theta+pi/(4*i)), sin(theta)],[0 0 r*sqrt(theta+pi/(8*i)) r*sqrt(theta+pi/(8*i))], 'g');
                hold on;
            end;    
%            X3 = cos(theta) ;
%            Y3 = sin(theta);
%            Z3 = R.* sqrt(theta);
            for theta=pi/2:pi/(4*i): (pi-pi/(4*i)),
                fill3([cos(theta), cos(theta+pi/(4*i)),cos(theta+pi/(4*i)), cos(theta)], [2-sin(theta), 2-sin(theta+pi/(4*i)),2-sin(theta+pi/(4*i)), 2-sin(theta)],[0 0 r*sqrt(theta) r*sqrt(theta)], 'g');            
                hold on;
            end;   
%            X4 = cos(A) ;
%            Y4 = 2-sin(A);
%            Z4 = R.* sqrt(A);
%            hold off;
%            mesh(X3,Y3,Z3)
%            hold on;
%            mesh(X4,Y4,Z4)
%            mesh(X3,Y3,Z3)
%            mesh(X4,Y4,Z4)
%mesh(X1,Y1,Z1)
%mesh(X2,Y2,Z2)
axis([-1 1 0 2 0 3])
            pause(1);
            hold off;
end;