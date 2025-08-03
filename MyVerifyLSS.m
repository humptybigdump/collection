function [XX,iter] = MyVerifyLSS(A,b)
XX = NaN;        % initialization
R = inv(mid(A)); % approximate inverse
xs = R*mid(b);   % approximate solution
B = eye(dim(A))-R*intval(A); % iteration matrix
Z = R*(b-A*intval(xs));
X = Z; iter = 0;
while iter<15
iter = iter+1;
Y = X*infsup(0.9,1.1) + eps*infsup(-1,1);
X = Z+B*Y; % interval iteration
if all(in0(X,Y)), XX = xs + X; return; end
end
end

