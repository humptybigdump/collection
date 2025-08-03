
rng(123)
%%
%a) die bedingten Verteilungen sind auch jeweils Normalverteilungen
% bauen wir hier die Funkionen, die uns aus diesen Verteilungen ziehen

mu=0; s=1; korr=0.7;

f2_geg_1 = @(x_1) normrnd(mu + korr*s*s/s^2*(x_1-mu), sqrt(s^2-(korr*s*s)^2/s^2))
f1_geg_2 = @(x_2) normrnd(mu + korr*s*s/s^2*(x_2-mu), sqrt(s^2-(korr*s*s)^2/s^2))

%b) 

Laenge=1100;
Chain=zeros(Laenge,2);

%Initialisiere abwegig, um künstlich eine längere Einbrennphase zu erzeugen
Chain(1,:)=[10 10];

for stelle=2:Laenge,
    Chain(stelle,1)=f1_geg_2(Chain(stelle-1,2));
    Chain(stelle,2)=f2_geg_1(Chain(stelle,1)); % also immer 
    %auf den aktuellst möglichen bedingen
end

%c) 
%i)

figure
subplot(2,1,1)
plot(Chain(:,1),'.-')
title('Erste Koordinate')
xlabel('Iteration')
ylabel('Realisation')

subplot(2,1,2)
plot(Chain(:,2),'.-')
title('Zweite Koordinate')
xlabel('Iteration')
ylabel('Realisation')

%% ii) iii)
%Einbrennphase wegstreichen
Chain(1:50,:)=[];

h1=figure;
h2=figure;
for welcher=[1,2]
figure(h1)
subplot(1,2,welcher)
[y,x]=ksdensity(Chain(:,welcher));
plot(x,y); hold on;
%zeichne noch eine Gauß-Dichte mit gleichen ersten beiden Momenten ein
plot(x,normpdf(x,0,1),'k--');   
set(gca,'xlim',[-4,4])
title(['Dichte Koordinate ',num2str(welcher)]);
legend('Kerndichte', 'N(0,1)','Location','East')

figure(h2)
subplot(2,1,welcher)
autocorr(Chain(:,1),60)
title(['Autokorrelationsfunktion Koordinate ',num2str(welcher)]);

end %welcher

% d)

cov(Chain)
