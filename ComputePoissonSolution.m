clear 
close all

%% the general PDE is -\div(c \grad u) + au = f
% so we set c=-1, f=0 and a=0 to get the Poisson problem
c = -1;
a = 0;
f = 0;
%% We define the region of interest
% We actually consider ellipses. This corresponds to 4 in the first entry.
% The other important entries are lengths of the semiaxes
% Outside ellipse
e1 = [4;0;0;1.5;1.5;0]; 
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
%% We construct the grid and solve the PDE
numberOfPDE = 1;
model = createpde(numberOfPDE);
geometryFromEdges(model,dl);
% This denotes the Dirichlet boundary data on the outside.
% We cannot pose Dirichlet and Neumann data on one boundary - Matlab cannot
% handle this kind of nonstandard Cauchy problem. Therefore we have to do
% it like this.
myufunction = @(location,state)0 + location.y;
% myufunction = @(location,state)0 + location.y.^2.*location.x.^3;
% homogenous Neumann boundary on the inside
applyBoundaryCondition(model,'neumann','Edge',5:8,'g',0,'q',0);
% Dirichlet data on the outside
applyBoundaryCondition(model,'dirichlet','Edge',1:4,'u',myufunction);
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
% pdeplot(p,e,t,'XYData',u,'Mesh','on')


