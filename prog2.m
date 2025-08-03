%Eigenvalue computation for 1D interface problem
%p=p1 on (0,a) and (b,1), p=p2 on (a,b)
%q: constant
%ENCLOSURE

clear
intvalinit('displaymidrad')
tic
p1=intval('1');
p2=intval('2');
a=intval('0.3');
b=intval('0.6');
mu=intval('1');
q=intval('1');
pi=acos(intval('-1'))
N=20;

A=intval(zeros(2*N+1,2*N+1));

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

[V,E]=eig(mid(A));
Evec=diag(E);
[ES,I]=sort(Evec);

v1=V(:,I(1));

[L,X]=verifyeig(A,ES(1),v1);
L

toc

