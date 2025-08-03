function [t,z,y]=heatconduct
% simple one-dimensional heat conduction problem. 
% The resulting PDE is solved numerically by method of lines 
% using a built-in ODE-solver.
% The ODE is of the form B*yp = f(t,y), with mass matrix B

% define spatial domain z: equidistant grid 
z=linspace(0,.1,51)';  % m
dz=z(2)-z(1);
% initial temperature profile: a Gaussian
T0=500;   % base temperature
DT=-100;  % temperature increase at maximum
w=.02;    % width of initial Gaussian
zm=0.1;   % center of initial Gaussian
y0=(z-zm)/w; 
y0=T0 + DT*exp(-y0.^2);
% material parameters. They are assumed to be constant along the profile. 
lam=.1;          % thermal conductivity  (W/(m*K))
cv=2.5*8.3145;   % specific heat capacity (J/(kg*K)) 
rho=1;           % density (kg/m3)

% call ODE solver
opts=odeset('RelTol',1e-3,'AbsTol',1e-2,'Mass',@mass);
[t,y]=ode23t(@hcode,[0,15],y0,opts);

  function f=hcode(t,y)
    f=lam./(rho.*cv)*del2(y,dz);  % del2: discrete approximation of second derivative 
    
    % Dirichlet-type boundary condition: Temperature at boundary(-ies) does not change
    f(1)=0;  
    % Neumann boundary conditions: specify derivatives at end. Mass matrix hass to be updated 
    f(end)=y(end)-y(end-1);
    
    
    % mixed boundary conditions: Dirichlet at left, 
    %   (for this option, Mass matrix B has to be set accordingly, see below)
    %f(1)=y(1)-T0;
    %ft(end)=-(y(end)-y(end-1))/dz-10*(y(end)-350);
  end

  function B=mass(t,y)
    B=eye(length(y));
    B(end,:)=0;
  end
end
