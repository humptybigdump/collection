% Titlle: Quadratic-Linear Programming for Mean Variance Portfolio Analysis
%The first formulation of the criterion function for mean-variance portfolio selection model.
%Defines the expected mean return, net of variance costs, which is to be maximized.

function [z] = dcri1(x, P) 

global mu; global sigma;
THETA=P(1);
BETA=P(2);
N=P(3);

z=0;
for i=1:N;
    temp=0;
    for j=1:N;
        temp=temp+BETA*sigma(i,j)*x(j);
    end;
    z=z+mu(i)*x(i)-0.5*x(i)*temp;
end;
%compact formaulation
%z=-(mu'*x-0.5*beta*x'*sigma*x;
z=-z;           % Matlab only has a subroutine to solve constrained MINIMIZATION problems.
                % We solve a maximization problem by minimizing the negative of the objective function.