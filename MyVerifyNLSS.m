function XX = MyVerifyNLSS(f,xs)
n=length(xs);
XX = NaN; % initialization
y = f(gradientinit(xs));
R = inv(y.dx); % approximate inverse of J_f(xs)
Y = f(gradientinit(intval(xs)));
Z = -R*Y.x; % inclusion of -R*f(xs)
X = Z ;
iter = 0;
while iter<15
  iter = iter+1;
  Y = hull( X*infsup(0.9,1.1) + 1e-20*infsup(-1,1) , 0 );
  YY = f(gradientinit(xs+Y)); % YY.dx inclusion of J_f(xs+Y)
  X = Z + (eye(n)-R*YY.dx)*Y; % interval iteration
  if all(in0(X,Y))
      XX = xs + X;
      return; 
  end
end
end
