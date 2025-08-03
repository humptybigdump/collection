function DirichletData = getDirichletData(u,p,e,N,side)
% This functions gets the Dirichlet data on the circle side.

% Depending on the boundary labels we have to put the data on the
% boundary together.
% e(5,:) indicates to which boundary the edges belong
if side == "outside"
    boundarylabels = 1:4;
elseif side == "inside"
    boundarylabels = 5:8;
else
    error("This is not a permitted side")
end
all_of_e = e(5,:);
% already ordered
all_edge = [];
for ii = boundarylabels
    % only on the boundary ii
    nr_ii = all_of_e==ii;
    only_ii_e = e(:,nr_ii);
    % sort them in a counter clockwise order(when looking at the domain)
    [~,b] = sort(only_ii_e(3,:));
    % put 'em together
    all_edge = [all_edge, only_ii_e(:,b)];
end

points_on_circle = p(:,all_edge(1,:));
points_on_circle(2,:) = 1i*points_on_circle(2,:);
cnum = sum(points_on_circle,1);
x_or = angle(cnum);
y_or = u(all_edge(1,:));
% We run into problems when we want to interpolate at points that are not defined
% within the actual grid. So we extend the periodic data... periodically:
% N = 2^11;
phi = linspace(-pi,pi-2*pi/N,N);
DirichletData = interp1([x_or-2*pi, x_or,x_or+2*pi],[y_or,y_or,y_or],phi);
end

