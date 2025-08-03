%%% UE VII Aufgabe 1 %%%

% 1a)
% Skizze der A-priori-Dichte
a=2;
b=4;
C=20;

theta=0:0.001:1;
f=zeros(numel(theta),1);
for i=1:numel(theta);
    f(i)=theta(i).^(a-1)*(1-theta(i)).^(b-1);
end
%f = @(theta) theta.^(a-1)*(1-theta).^(b-1);

plot(theta,f)
xlabel('$\theta$','Interpreter','LaTex');
ylabel('$\pi_0(\theta)$','Interpreter','LaTex');
title('A-priori-Dichte');