intvalinit('displayinfsup')
n=9
A=intval(zeros(n,n));
for i=1:n
    for j=1:n
        A(i,j)=intval('1')/(i+(j-1)*n);
    end
end
b=A*ones(n,1);
MyVerifyLSS(A,b)