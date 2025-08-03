% Exercise 1 & 2
format longe 
n = 8; C = triu(tril(ones(n),1));
H = hadamard(n); A = H'*C*H/8;
[V,D] = eig(A);

e = diag(D)
plot(real(e),imag(e),'b*')
grid on
hold on
plot(1,0,'r.', 'MarkerSize', 15)

intvalinit('displaymidrad')
[EP,I]=sort(e);
[L1,X1]=verifyeig(A,EP(1),V(:,I(1))); % Exercise 1 (same for other EP(k))
L1
[Lall,Xall] = verifyeig(A,mean(e),V); % Exercise 2 
Lall

% Exercise 3
W = wilkinson(21);
[V,D] = eig(W);
e = diag(D)
[EP,I]=sort(e);

% disp('Enclosure of each eigenvalue:')
% for k=1:21
%   k
%   [L,X] = verifyeig(W,e(k),V(:,I(k))) ;
%   disp(L)  
% end
disp('Enclosure of the 20th and 21st eigenvalues as a cluster:')
e2=mean([EP(20) EP(21)]);
V2=V(:,[I(20) I(21)]);
[L2,X2] = verifyeig(W,e2,V2);
disp(L2)

[L20,X20] = verifyeig(W,EP(20),V(:,I(20)));
[L21,X21] = verifyeig(W,EP(21),V(:,I(21)));
disp('Enclosure of the 20th eigenvalue:')
disp(L20)
disp('Enclosure of the 21st eigenvalue:')
disp(L21)
distance = inf(L21)-sup(L20)
if distance>0
    disp('The 20th und 21st eigenvalues are separated.')
end
