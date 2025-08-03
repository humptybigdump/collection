function model = applyDirichlet(model,side,val)
% This script applies Dirichlet data to the Matlab PDE model named model on
% the inside or outside of the annulus.
% Input:    - model, a Matlab PDE model
%           - side: a string. Either "inside" or "outside"
%           - val: a vector containing the Dirichlet data, which are
%           applied counterclockwise on the boundary starting at the point
%           (-R,0), where R denotes the radius of the circle.

    
    if side == "outside"
        boundarylabels = 1:4;
    elseif side == "inside"
        boundarylabels = 5:8;
    else
        error("This is not a permitted side")
    end
    % The problem is that Matlab doesn't know how to set values at a
    % boundary. Therefore we have to create our own function handle via
    % interpolation
    myfunction = @(location,state) fun(location,state,val);
    applyBoundaryCondition(model,'dirichlet','Edge',boundarylabels,'u',myfunction);
end

