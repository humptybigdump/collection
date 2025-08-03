%Eigenvalue computation for 1D interface problem
%p=p1 on (0,a) and (b,1), p=p2 on (a,b)
%q: constant
%APPROXIMATION

clear
tic
p1=1; p2=2; a=0.3; b=0.6; mu=1; q=1;
N=100;

A=zeros(2*N+1,2*N+1);

for n=-N:N
    A(n+N+1,n+N+1)=p1*(2*pi*n+mu)^2*(a+1-b)+p2*(2*pi*n+mu)^2*(b-a)+q;
end

for n=-N:N-1
    for m=n+1:N
        A(n+N+1,m+N+1)=(2*pi*n+mu)*(2*pi*m+mu)/(2*pi*1i*(n-m))*...
        (p1-p2)*(exp(2*pi*1i*(n-m)*a)-exp(2*pi*1i*(n-m)*b));
        A(m+N+1,n+N+1)=conj(A(n+N+1,m+N+1));
    end
end

[V,E]=eig(A);
Evec=diag(E);
[ES,I]=sort(Evec);

M=1000;
u=zeros(M,1);
x=linspace(0,1,M);
%U=[V(:,I(1)) V(:,I(2)) V(:,I(3)) V(:,I(4)) V(:,I(5))];
v1=V(:,I(1));
error=norm(A*v1-ES(1)*v1)

for k=-N:N
    for t=1:M
        u(t)=u(t)+conj(v1(k+N+1))*exp((2*pi*k+mu)*1i*x(t));
    end
end
 
figure(1);
plot(x,real(u));
xlim([0,1]) 

figure(2);
plot(x,imag(u));
xlim([0,1]) 

toc

