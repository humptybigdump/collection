% Computer-assisted Proof for 1D Emden's equation
% -u'' = u^2 in (0,1)
% u = 0 on boundary

clear

Nd = 100; MAXIT = 20; d0 = 1d-3;

% approximate solution
IREND=50;
epsilon=1.0E-8

h=1/(Nd+1)
U=zeros(Nd,1); newU=zeros(Nd,1);
J=zeros(Nd); RH=zeros(Nd,1); 
xvec=zeros(Nd+2);
uvec=zeros(Nd+2);

%Set initial values
for k=1:Nd
    U(k)=10*sin(pi*k*h);
end

for i=1:IREND
    %Set Matrix
    for k=2:Nd-1
        J(k,k)=h*(U(k)+U(k-1)/6+U(k+1)/6)-2/h;
    end
    J(1,1)=h*(U(1)+U(2)/6)-2/h;
    J(Nd,Nd)=h*(U(Nd)+U(Nd-1)/6)-2/h;
    
    for k=2:Nd
        J(k,k-1)=(h/6)*(U(k-1)+U(k))+1/h;
    end
    
    for k=1:Nd-1
        J(k,k+1)=(h/6)*(U(k+1)+U(k))+1/h;
    end
        
    %Set right-hand vector    
    for k=2:Nd-1
        RH(k)=(h/12)*(U(k-1)^2+6*U(k)^2+U(k+1)^2+2*U(k-1)*U(k)+2*U(k)*U(k+1))...
               -(1/h)*(2*U(k)-U(k-1)-U(k+1));        
    end
    RH(1)=(h/12)*(6*U(1)^2+U(2)^2+2*U(1)*U(2))-(1/h)*(2*U(1)-U(2)); 
    RH(Nd)=(h/12)*(U(Nd-1)^2+6*U(Nd)^2+2*U(Nd-1)*U(Nd))-(1/h)*(2*U(Nd)-U(Nd-1)); 
    
    newU=U-J\RH;
    
  if(norm(newU-U)<norm(U)*epsilon)
      disp('converged')
      X = (0:Nd+1)'*h;
      Y = [0 newU' 0]';
      figure(1)
      plot(X,Y,'k')
      grid on
      break;
  else
      U=newU;
      i=i+1;
  end
end

if(i==IREND)
    disp('did not converge...')
    return
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
uh=newU;

one = intval('1'); two = intval('2'); unit = intval('[-1,1]');
Pi = intval('pi')

h = one/(Nd+1); 
Ch = h/Pi;
g = ones(Nd-1,1); % generating matrices D and L
D = (2*diag(ones(Nd,1)) - diag(g,1) - diag(g,-1))/h;
L = (4*diag(ones(Nd,1)) + diag(g,1) + diag(g,-1))*h/6;

G = D - 2*L*uh;

r=intval(zeros(Nd,1));
r(1)=(h/2)*uh(1)^2+(h/12)*(uh(2)^2+2*uh(1)*uh(2));
r(Nd)=(h/2)*uh(Nd)^2+(h/12)*(uh(Nd-1)^2+2*uh(Nd)*uh(Nd-1));
for i=2:Nd-1
    r(i)=(h/2)*uh(i)^2+(h/12)*(uh(i+1)*(uh(i+1)+2*uh(i))+uh(i-1)*(uh(i-1)+2*uh(i)));
end
r = r-D*uh; % residual vector

u = unit*eps*ones(Nd,1); alpha = eps; % initial candidate set
d=intval(zeros(Nd,1));

for k = 1:MAXIT
    fprintf('%d ',k);
    u = (1+d0)*u; alpha = (1+d0)*alpha; % inflation of candidate set
    
    w = uh + u;
    s = sqrt(intval(w'*(L*w)));
    
    d(1)=(h/2)*u(1)^2+(h/12)*(u(2)^2+2*u(1)*u(2));
    d(Nd)=(h/2)*u(Nd)^2+(h/12)*(u(Nd-1)^2+2*u(Nd)*u(Nd-1));
    for i=2:Nd-1
        d(i)=(h/2)*u(i)^2+(h/12)*(u(i+1)*(u(i+1)+2*u(i))+u(i-1)*(u(i-1)+2*u(i)));
    end
    d = r + d + unit*Ch*alpha*(2*s+Ch*alpha);
    
    v = G\d; % next Uh
    beta = sqrt(sup(Ch*(s*(s+2*Ch*alpha)+(Ch*alpha)^2))); % next U*
    res = sum(in(v,u)); % checking contraction: finite
    if beta <= alpha % infinite part
       res = res + 1;
    end
    if res == Nd+1 % verification check
       disp('Verification has been completed!');
       fprintf('max(Uh): %d\n',max(mag(v))); % maximum-norm of Uh
       fprintf(' alpha: %d\n',beta); % H^1_0-norm of U*
       X = (0:Nd+1)'*mid(h);
       Y = [0 w' 0]';
       figure(2)
       plot(X,Y,'k')
       grid on
       break;
    end
    if k == MAXIT
       disp('Verification was not successful...');
       break;
    end
    u = v; alpha = beta; % preparing next step
end
