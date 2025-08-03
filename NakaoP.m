Nd = 50; MAXIT = 20; d0 = 1d-3;
one = intval('1'); 
Pi = 4*atan(one); % oder Pi = intval('pi')
c = Pi;
two = intval('2'); unit = intval('[-1,1]');
h = one/(Nd+1); Ch = h/Pi;
g = ones(Nd-1,1); % generating matrices D, L
D = (2*diag(ones(Nd,1)) - diag(g,1) - diag(g,-1))/h;
L = (4*diag(ones(Nd,1)) + diag(g,1) + diag(g,-1))*h/6;
x = h*(1:Nd)';
f = (2*sin(Pi*x)-sin((x-h)*Pi)-sin((x+h)*Pi))/(h*Pi^2);
uh = mid(D - c*L)\mid((Pi-c/Pi)*f); % approximate solution
r = (c*L-D)*uh + (Pi-c/Pi)*f; % residual vector
u = unit*eps*ones(Nd,1); alpha = eps; % initial candidate set
for k = 1:MAXIT
    fprintf('%d ',k);
    u = (1+d0)*u; alpha = (1+d0)*alpha; % inflation
    d = r + c*L*u + unit*c*Ch*alpha*sqrt(h*two/3);
    v = D\d; % next Uh
    w = uh + u;
    s = c*sqrt(intval(w'*(L*w))) + abs(Pi-c/Pi)/sqrt(two);
    beta = sup(Ch*(s+c*Ch*alpha)); % next U*
    res = sum(in(v,u)); % checking contraction
    if beta <= alpha % infinite part
       res = res + 1;
    end
    if res == Nd+1 % verification check
       disp('Verification has been completed!');
       fprintf('max(Uh): %d\n',max(mag(v))); % maximum-norm of Uh
       fprintf('alpha: %d\n',beta); % H^1_0-norm of U*
       Y = [0 w' 0]';
       X = (0:Nd+1)'*mid(h);
       plot(X,Y,'k')
       break;
    end
    if k == MAXIT
       disp('Verification was not successful...');
    end
u = v; alpha = beta; % preparing next step
end