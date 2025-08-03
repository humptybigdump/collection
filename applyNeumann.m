function model = applyNeumann(model,side,val)
% This script applies Neumann data to the Matlab PDE model named model on
% the inside or outside of the annulus.
% Input:    - model, a Matlab PDE model
%           - side: a string. Either "inside" or "outside"
%           - val: a vector containing the Neumann data, which are
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
    % Mind the -1 factor in front of the function. This has to be here,
    % since Matlab uses negative coefficients for the Neumann data since
    % c=-1... This is different for Dirichlet data...
    myfunction = @(location,state) -1*fun(location,state,val);
    applyBoundaryCondition(model,'neumann','Edge',boundarylabels,'g',myfunction,'q',0);
end


