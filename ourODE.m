function [t,y]=ourODE(f,t0,y0,h,tend)
% numerical solution of an IVP-ODE yp = f(t,y), y(t0)=y0
% f a function handle f(t,y)
% t0,tend: initial and final time steps, resp.
% y0: initial value for y(t), y0=y(t0)
% h: time step for the solution 

yl=y0;tl=t0;
nsteps=ceil((tend-t0)/h);
t=zeros(nsteps,1);y=zeros(nsteps,numel(y0));
t(1)=t0; y(1,:)=y0;
for i=2:nsteps
	yr = yl + h.*f(tl,yl);
	y(i,:)=yr; tr=tl+h; t(i)=tr;
	yl=yr; tl=tr; 
end
