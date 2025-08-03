
%%
% definiere zun�chst die Funktion

f=@(x1,x2,x3) (1-cos(x1).*cos(x2).*cos(x3))/(8*pi^3);

% wie gro�e soll die Stichprobe werden
stichpr_n=10000;

% wir entscheiden uns in allen F�llen f�r die Verwerfungsmethode

%%
% a)

% Man erkennt, dass die Funktion f(x1,0,0) ihr Maximum bei x1=pi hat und 
% dass f(pi,0,0)=2/(8*pi^3)~=0.008 ist.

%Als Vorschlagsdichte f�r x1 nehmen wir eine Gleichverteilung �ber [0,2pi]
% diese hat auf dem gesamten Tr�ger eine Dichte von 1/(2*pi) > 2/(8*pi^3)
% die Gr��e k aus Kap 5.6 ist also

k=(2/(8*pi^3)) / (1/(2*pi));   % Skalierungsparameter

% 
% t=0:0.01:2*pi;
% plot(t,(1-cos(t))/(8*pi^3))
% hold on
% plot(t, ones(numel(t),1)*1/(2*pi))
% fun=@(x) (1-cos(x))/(8*pi^3);
% q=integral(fun,0,2*pi)

% Z�hle die Versuche
Versuche=0;
Erfolge=0;
X_stich=zeros(stichpr_n,1);
tic(); %Zeit stoppen
while Erfolge<stichpr_n,
   Versuche=Versuche+1;
    x=rand(1,1)*2*pi;          % erzeugt Zufallsvar aus U(0,2pi)
   if rand(1,1)<f(x,0,0)/(2*pi)/k,  % vergleicht Dummy mit p=h(x)/(p*g(x))
       Erfolge=Erfolge+1;
       X_stich(Erfolge)=x;
   end
end
Zeit=toc();

disp(['F�r eine Stichprobenl�nge von n=',num2str(Erfolge),...
    ' waren ',num2str(Versuche),' Versuche notwendig']);
disp(['Verh�ltnis ',num2str(Erfolge/Versuche)]);
    
disp(['Die Schleifen-Version ben�tigte ',num2str(Zeit),' Sekunden.']);

% Anmerkung: Die Programmierung �ber eine Schleife ist sehr langsam und
% hier nur aus didaktischen Gr�nden so gemacht. Man w�rde eigentlich direkt
% viele Vorschl�ge erzeugen und dann unter Ausnutzung von
% Matrixschreibweise die Stichprobe zuschneiden.
% Also alternativ:

tic();
X_stich2=rand(stichpr_n*50,1)*2*pi;    % 50 kommt aus Verh�ltnis Erfolge/Versuche
dummy=rand(stichpr_n*50,1);
X_stich2(f(X_stich2,0,0)/(2*pi)/k<dummy)=[];  
if length(X_stich2)>stichpr_n, X_stich2=X_stich2(1:stichpr_n); end;

Zeit=toc();
disp(['Die Matrix-Version ben�tigte ',num2str(Zeit),' Sekunden.']);


% jetzt mache noch kerndichtesch�tzungen beider Stichproben

h1=figure;
[y,x]=ksdensity(X_stich);
plot(x,y); hold on
[y,x]=ksdensity(X_stich2);
plot(x,y);

legend('X_1','X_2');


%%
%b)
%Wir nehmen der Einfachheit halbe wieder uniforme unabh�ngige Vorschl�ge
% �ber [0,2pi]^2
% diese hat auf dem gesamten Tr�ger eine Dichte von 1/(4*pi^2) > 2/(8*pi^3)
% die Gr��e k aus Kap 5.6 ist also
k=(2/(8*pi^3)) / (1/(4*pi^2));


%Wir verwenden nur die Matrixversion

tic();
X_stich3=rand(stichpr_n*4000,2)*2*pi;   % erzeugt n*4000 Zufallszahlen aus R(0,2pi) jeweils f�r x1 und x2 (4000 um sicherzustellen, dass gen�gend Annahmen)
dummy=rand(stichpr_n*4000,1);
X_stich3(f(X_stich3(:,1),X_stich3(:,2),0)/(4*pi^2)/k<dummy,:)=[];


if length(X_stich3(:,1))>stichpr_n, X_stich3=X_stich3(1:stichpr_n,:); end;  % sollten mehr als n Zufallszahlen angenommen worden sein, wir der Vektor einfach abgeschnitten

Zeit=toc();
disp(['Die Matrix-Version f�r b) ben�tigte ',num2str(Zeit),' Sekunden.']);

figure;
plot(X_stich3(:,1),X_stich3(:,2),'.')
title('Streudiagramm der Stichprobe aus Aufg 3 b)')
axis square
set(gca,'xlim',[0,2*pi]);
set(gca,'ylim',[0,2*pi]);
