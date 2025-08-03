

                   
 
% Übergangsdichte (hier Gaußkern)
sigma = 1;                                
Ziehe_Uebergang = @(x_minus_1) normrnd(x_minus_1,sigma); 
 
% Ziel"dichte" (nur auf Proportionalität eine Dichte)
p = @(x) (x>2).*exppdf(x,1);   

Laenge=21000; %wieviele erzeugen (inkl. Einbrennphase)
Chain=zeros(Laenge,1);
%Init mit etwas absurdem Wert
Chain(1)=50;

akzep=zeros(Laenge,1); %ein zaehler

for stelle=2:Laenge,
    vorschlag=Ziehe_Uebergang(Chain(stelle-1));
    % wir haben einen symmetrischen Kern; daher betrachten wir nur das
    % Verhältnis p(vorschlag)/p(alt)
    if rand(1,1)< p(vorschlag)/p(Chain(stelle-1)),  %akzeptiere
        Chain(stelle)=vorschlag;
        akzep(stelle)=1;
    else % nimm den alten
        Chain(stelle)=Chain(stelle-1);
    end
   
end

%% 
% i)
figure
plot(Chain(:,1),'.-')
title('Erzeugte Realisationen')
xlabel('Iteration')
ylabel('Realisation')

Einbrenn=1000;
disp(['Akzeptanzverhältnis ',num2str(sum(akzep(Einbrenn+1:end))/(Laenge-Einbrenn))])
% Einbrennphase wegstreichen
Chain(1:1000)=[];

%%
% ii)

figure
autocorr(Chain(:,1),150)
title(['Autokorrelationsfunktion']);

%%
% iii)
figure
[y,x]=ksdensity(Chain(:,1));
plot(x,y); hold on;
%zeichne noch die theoretische Dichte
cons=1-expcdf(2,1);
plot(x,p(x)/cons,'k--');   
set(gca,'xlim',[0,6])
title(['Kerndichteschätzung']);
legend('Kerndichte', 'wahr','Location','East')



