%%% UE VIII Aufgabe 1 %%%

% Plot der Gütefunktion

%% 1a) Rechteck-Verteilung
n=100;
alpha=0.05;
b_0=4;
beta=3:0.01:7;

c=normcdf(1-alpha,0,1);

G=zeros(numel(beta),1);
for i=1:numel(beta);
G(i)=1-normcdf((b_0/2-beta(i)/2+c*sqrt(b_0^2/(12*n)))/sqrt(beta(i)^2/(12*n)));
end

figure(1)
plot(beta,G)
xlabel('$\beta$','Interpreter','LaTex');
ylabel('$G(\beta)$','Interpreter','LaTex');
title('Gütefunktion (einseitiger Test)');


%% 1b) Poisson-Verteilung

n=100;
alpha=0.05;
mu_0=4;
mu=1:0.01:7;

c=normcdf(1-alpha,0,1);

F=zeros(numel(mu),1);
for i=1:numel(mu);
F(i)=1-normcdf((mu_0-mu(i)+c*sqrt(mu_0/n))/sqrt(mu(i)/n))+normcdf((mu_0-mu(i)-c*sqrt(mu_0/n))/sqrt(mu(i)/n));
end

figure(2)
plot(mu,F)
xlabel('$\mu$','Interpreter','LaTex');
ylabel('$G(\mu)$','Interpreter','LaTex');
title('Gütefunktion (zweiseitiger Test)');

% Gütefunktion des einseitigen Tests mit
% H_0: mu<=mu_0
% H_1: mu>mu_0

R=zeros(numel(mu),1);
for i=1:numel(mu);
R(i)=1-normcdf((mu_0-mu(i)+c*sqrt(mu_0/n))/sqrt(mu(i)/n));
end

figure(3)
plot(mu,R)
xlabel('$\mu$','Interpreter','LaTex');
ylabel('$G(\mu)$','Interpreter','LaTex');
title('Gütefunktion (einseitiger Test)');

