%%% UE VIII A3 %%%

% Gütefunktion des zweisetigen Tests
pi=0:0.01:1;
g=zeros(numel(pi),1);
for i=1:numel(pi);
    g(i)=1-binocdf(6,10,pi(i))+binocdf(0,10,pi(i));
end

alpha=ones(numel(pi),1)*0.0395;

subplot(2,1,1)
plot(pi,g)
xlabel('$\pi$','Interpreter','LaTex');
ylabel('$G(\pi)$','Interpreter','LaTex');
title('Gütefunktion');
hold on
plot(pi,alpha,'--')

subplot(2,1,2)
plot(pi(25:40),g(25:40))
xlabel('$\pi$','Interpreter','LaTex');
ylabel('$G(\pi)$','Interpreter','LaTex');
title('Gütefunktion (Ausschnitt');
hold on
plot(pi(25:40),alpha(25:40),'--')