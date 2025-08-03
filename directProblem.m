clear all
close all
%% Setup for the Matlab PDE toolbox
% the general PDE is -\div(c \grad u) + au = f
% so we set c=-1, f=0 and a=0 to get the Poisson problem
c = -1;
a = 0;
f = 0;
% We define the region of interest
% We actually consider ellipses. This corresponds to 4 in the first entry.
% The other important entries are lengths of the semiaxes
% Outside ellipse
rad = 1.5;
e1 = [4;0;0;rad;rad;0]; 
% Inside ellipse
e2 = [4;0;0;1;1;0]; 
% Both ellipses
ee = [e1 e2]; 
% Ellipse labels
lbls = char('outside','inside'); 
% Change to columns
lbls = lbls'; 
% Set formula
sf = 'outside-inside'; 
% Geometry now done
dl = decsg(ee,sf,lbls); 
% Plot the geometry
pdegplot(dl,'EdgeLabels','on')
% we construct the grid and solve the PDE
numberOfPDE = 1;
model = createpde(numberOfPDE);
geometryFromEdges(model,dl);
% This denotes the Dirichlet boundary data on the outside.
% We cannot pose Dirichlet and Neumann data on one boundary - Matlab cannot
% handle this kind of nonstandard Cauchy problem. Therefore we have to do
% it like this.

%% Direct problem
N = 2^11;
exact_f = @(x)exp(-sin(2*x)).*cos(4*x); %exact data - this is what we want to reconstruct
% exact_f = @(x)sin(x);
% Solve the problem:
% Neumann data on the inside
model = applyNeumann(model,"inside",0);
% Dirichlet data on the outside
phi = linspace(-pi,pi-2*pi/N,N);
vec = exact_f(phi);
model = applyDirichlet(model,"outside",vec);
% Usually we need two steps: Creating the grid and approximiating the PDE.
% In the special case of the 2nd order elliptic PDE that we treat here, we
% can combine these steps by adaptmesh. This even uses an adaptive grid
% refinement up to a certain level.
[u,p,e,t] = adaptmesh(dl,model,c,a,f,'tripick','circlepick','maxt',40000,'par',1e-5);
% transpose just to get the dimensions right
u = u.';
% plot the solution
figure;
pdeplot(p,e,t,'XYData',u,'ZData',u,'Mesh','off');
y = getDirichletData(u,p,e,N,"inside");
% Noise level
delta = 0.05;
noise = (rand(1,length(y),1) - rand(1,length(y),1));
noise = noise./max(abs(noise));
g_delta = delta*noise*max(abs(y)) + y;
figure
plot(phi,g_delta);
title(strcat("The given data on the inner circle. Noise level: ",num2str(delta*100),"%"),'Fontsize',14);

%% Inverse Problem
% Up to now: Given: - g_delta. Aim: Reconstruct f.

%% HINTS: 
% Use the command 
%   delete(model.BoundaryConditions)
% before applying the two boundary conditions for the subproblems. Otherwise they stack up



% --> use applyDirichlet(model,side,val) to set the Dirichlet data in val on the boundary defined by side
%     Here: model - is the PDE model from above
%           side - "inside" or "outside" depending on the boundary
%           val - the vector which has the Dirichlet data in it, which is
%           applied counterclock wise, beginning at (-R,0) ( R depending on
%           side)

% --> use applyNeumann(model,side,val) the same as above only for Neumann
% data. The -1 has already been included! Use it as the Dirichlet function

% --> use getDirichletData(u,p,e,N,side), to get the Dirichlet data.
%     Here: (u,p,e) - comes from the solution of adaptmesh
%     (use it as in the direct problem above)
%           N - is 2^11
%           side - "inside" or "outside" depending on the boundary

% --> use getNeumannData(model,rad,c,a,f) to get the Neumann data on the
% outer circle. 
%     Here: model - is the PDE model from above
%           rad - is the radius from above (1.5)
%           (c,a,f) - are the coefficients in from the PDE ( (-1,0,0)).














