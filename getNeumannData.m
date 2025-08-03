function Neumann_data = getNeumannData(model,rad,c,a,f)
% This script gets the Neumann data of the Matlab PDE model at the radius
% rad. I did not see an alternative way to get these data. Therefore we
% solve the PDE once more, get the gradient and do a scalar multiplication
% with \hat x on the unit sphere... Alternatives???
    
    specifyCoefficients(model, 'm',0,...
                               'd',0,...
                               'c',c,...
                               'a',a,...
                               'f',f);
    generateMesh(model,'Hmax',7e-2);
    results = solvepde(model);
    N = 2^11;
    phi = linspace(-pi,pi-2*pi/N,N);
    xx = cos(phi);
    yy = sin(phi);
    vecF = [xx;yy];
    [gradx,grady] = evaluateGradient(results,(rad-1e-6)*xx,(rad-1e-6)*yy);
    Gradu = [gradx.';grady.'];
    Neumann_data = dot(vecF,Gradu);
    % Otherwise we cannot continue with adaptmesh
    model.Mesh = [];
    delete(model.EquationCoefficients)
end

