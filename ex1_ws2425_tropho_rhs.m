function xdot = ex1_ws2425_tropho_rhs(t,x,p,u)
%
mu_max   = p.mu_max;
Ks       = p.Ks;
V        = p.V;
p1       = p.p1;
p2       = p.p2;
mS       = p.mS;
%
u1       = u(t,x,p); %Zuordnung von Eingang u(t) zur aktuellen Zeit t
                     %u1       = u1*(1-stepfun(t,50));
%
mu       = mu_max*x(2)/(Ks*V+x(2));
f_tropho = [mu*x(1);-1/p1*mu*x(1)-mS*x(1)+p2*u1];
xdot     = f_tropho;
