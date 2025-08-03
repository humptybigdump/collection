function pdex4
%PDEX4  Example for PDEPE
%   This example illustrates the solution of a system of partial differential
%   equations with PDEPE. 
%
%   In the form expected by PDEPE, the equations are
%
%   |c| .*  D_  |u | = D_ | f(x,t,u,Du/Dx) | +  | s(x,t,u,Du/Dx) |
%           Dt         Dx 
%
%   In the form expected by PDEPE, the left bc is
%
%      |p(0,t,u)| +  |q(0,t)| .* | f(0,t,u,Du/Dx) | = 0
%
%   and the right bc is
%
%      |p(1,t,u)| +  |q(1,t)| .* | f(1,t,u,Du/Dx) | = 0
%
%   See the subfunctions PDEX4PDE, PDEX4IC, and PDEX4BC for the coding of the
%   problem definition.
%
%   See also PDEPE, FUNCTION_HANDLE.


m = 0;
x = -100:0.01:100;
t = 0:0.1:25;

sol = pdepe(m,@pdex4pde,@pdex4ic,@pdex4bc,x,t);
u1 = sol(:,:,1);

% Record dynamics

v = VideoWriter('FKPP.avi');
open(v);
fig = figure;
h = figure(1);
for i = 1:length(t)
plot(x,u1(i,:),'Linewidth',2);
axis([-50,50,-0.2,1.2]);
hold off;
writeVideo(v,getframe(h));
end
close(v);

function [c,f,s] = pdex4pde(x,t,u,DuDx)
c = 1;
f = DuDx;
s = (1-u) * u;
% s = (1-u) * u * (u-1/4);

% Bump-like initial condition

function u0 = pdex4ic(x)
u0 = 0.1*exp(-1/(1-x^2))*(1+sign(1-x^2));
% u0 = 1 - 0.1*exp(-1/(1-x^2))*(1+sign(1-x^2));
% u0 = (1+exp(x/sqrt(6)))^(-2) - 0.1*exp(-1/(1-(x+30)^2))*(1+sign(1-(x+30)^2));
% u0 = (1+exp(x/sqrt(2)))^(-1) + 0.1*exp(-1/(1-(x-30)^2))*(1+sign(1-(x-30)^2));


% Dirichlet boundary conditions

function [pl,ql,pr,qr] = pdex4bc(xl,ul,xr,ur,t)
pl = ul(1);
ql = 0;
pr = ur(1);
qr = 0;

