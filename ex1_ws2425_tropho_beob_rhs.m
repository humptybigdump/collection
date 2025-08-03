function xdot = ex1_ws2425_tropho_beob_rhs(t,x,p,u)
%
xsys  = x(1:2);
dxbeo = x(3:4);
xbeo  = dxbeo+p.xRN; 
%
mu_max   = p.mu_max;
Ks       = p.Ks;
V        = p.V;
p1       = p.p1;
p2       = p.p2;
mS       = p.mS;
%
u1       = u(t,xbeo,p); % Zustandsrückführung (Bestimmung delta_x und Addition Ruhelage uR in der Funktion u(t,x,p)        
%
mu       = mu_max*x(2)/(Ks*V+x(2));
f_tropho = [mu*x(1);-1/p1*mu*x(1)-mS*x(1)+p2*u1];

%
y        = x(1); % oder para.cT*xsys;
yR       = p.xRN(1);
dy       = y-yR;
du       = u1-p.u1R;
f_beob   = p.A*dxbeo + p.b*du + p.L*(dy-p.cT*dxbeo);

%
xdot     = [f_tropho;f_beob]; 
