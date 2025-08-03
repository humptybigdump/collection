Nd = 20; MAXIT = 20; d0 = 1d-3;
one = intval('1'); Pi = 4*atan(one);
c = Pi;
two = intval('2'); unit = intval('[-1,1]');
h = one/(Nd+1); Ch = h/Pi;

% generating matrices D, L
g = ones(Nd-1,1); 
D = 
L = 
x = h*(1:Nd)';
f = 
uh = mid(D - c*L)\mid((Pi-c/Pi)*f); % approximate solution
r =  % residual vector (blue part on p.9)
u = unit*eps*ones(Nd,1); alpha = eps; % initial candidate set
for k = 1:MAXIT
    fprintf('%d ',k);
    u = (1+d0)*u; alpha = (1+d0)*alpha; % inflation
    d = 
    v =  % next Uh
    w = uh + u;
    s = c*sqrt(intval(w'*(L*w))) + abs(Pi-c/Pi)/sqrt(two);
    beta =  % next U*
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