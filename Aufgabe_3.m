
%% 
%a) 
%Lege jeweils für 4 verschiedenen Stichprobengrößen Zeichnungen an
StichVek=[50, 60, 70, 80];
%Parameter der Pareto-Verteilung
alpha=3.5; %shape-parameter
c=5; %scale parameter

mu=alpha*c/(alpha-1);
var=alpha*c^2/((alpha-2)*(alpha-1)^2);

Wiederholungen=10000; % Anzahl der Monte-Carlo-Wiederholungen

h1=figure;
h2=figure;

% Quantilfunktion der Pareto-Verteilung
paretinv = @(p) c*(1-p).^(-1/alpha);

for stichpr_pos=1:length(StichVek),
   stichpr_n=StichVek(stichpr_pos);
   
   %erzeuge Stichprobe
   X_stich=paretinv(rand(Wiederholungen,stichpr_n));
   %berechne nun die Mittelwerte
   X_qu=mean(X_stich,2);      % entspricht 1/n*sum(xi) --> darauf ZGWS anwenden
   
   %nun plotten
   
   figure(h1)
   subplot(2,2,stichpr_pos)
   
   %kerndichteschätzung
   [y,x]=ksdensity(X_qu);
    
   plot(x,y); hold on;
   
   %zeichne noch eine Gauß-Dichte mit gleichen ersten beiden Momenten ein
   
   plot(x,normpdf(x,alpha*c/(alpha-1), sqrt(1/stichpr_n*alpha*c^2/((alpha-2)*(alpha-1)^2))),'k--');
   set(gca,'xlim',[0, alpha*c/(alpha-1)+5*sqrt(1/stichpr_n*alpha*c^2/((alpha-2)*(alpha-1)^2))])
   
   title(['Verteilung für n=',num2str(stichpr_n)]);
   
   % und noch einen qqplot
   figure(h2)
   subplot(2,2,stichpr_pos)
   qqplot(X_qu);
   title(['Verteilung für n=',num2str(stichpr_n)]);
   
   
   
end

%% 
% b) nun einmal Realisationen aus der Methode 5.6
%  Beachte, dass die Varianz einer U(0,1)-Verteilung 1/12 ist. 
% Der Erwartungswert ist 1/2
Y_stich=(12*mean(rand(Wiederholungen,12),2)-6);

figure;
   [y,x]=ksdensity(Y_stich);
   plot(x,y); hold on;
   
   %zeichne noch eine Gauß-Dichte mit gleichen ersten beiden Momenten ein
   
   %plot(x,normpdf(x,mean(Y_stich), std(Y_stich)),'k--');
   plot(x,normpdf(x,0,1),'k--');
   %set(gca,'xlim',[0, mean(Y_stich)+5*std(Y_stich)])
   
   title(['Skalierte Summe aus 12 U(0,1) Variablen']);
   
   figure
   qqplot(Y_stich)
