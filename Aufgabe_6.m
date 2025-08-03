

%a)
% muss einfach hingeschrieben werden

%b)

Wieviele=1000; %(Stichprobengröße)
theta=2.5; % parameter


%i)
% durch umkehren
rng(123)
U1=rand(Wieviele,2);
U1(:,2)=((U1(:,2).^(-theta./(theta+1))) .* U1(:,1).^(-theta)  - U1(:,1).^(-theta)  + 1 ).^(-1./theta); 

%ii)
% Algorithmus von marshall olkin 88; bzw. aus schönbucher rogge 2003
% parameter sind shape und scale der Gamma Verteilung bzw. theta der Clayton copula.
% shape ist = 1/theta, scale ist 1
rng(123)
Vs=gamrnd(1/theta, 1,1,Wieviele);
U2=((1-log(rand(2,Wieviele))*(diag(Vs.^(-1))) ).^(-1/theta))';

%iii)
rng(123)
U3=copularnd('Clayton',theta,Wieviele);



% jetzt mal Streudiagramme machen

h1=figure
labels={'i)','ii)','iii)'};
for welcher=[1 2 3]
subplot(2,2,welcher)
eval(['U=U',num2str(welcher),';'])
plot(U(:,1),U(:,2),'.');
axis square;
title(['Aufgabenteil ',char(labels(welcher))])
end
subplot(2,2,4)
text(0.2,0.8,'Mit U(0,1)-Rändern')
axis off



h2=figure
labels={'i)','ii)','iii)'};
for welcher=[1 2 3]
subplot(2,2,welcher)
eval(['U=U',num2str(welcher),';'])
plot(norminv(U(:,1)),norminv(U(:,2)),'.');
axis square;
title(['Aufgabenteil ',char(labels(welcher))])
end
subplot(2,2,4)
text(0.2,0.8,'Mit N(0,1)-Rändern')
axis off

