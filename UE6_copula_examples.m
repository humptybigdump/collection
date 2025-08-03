%% Bivariate Normalverteilung
% normalverteilte Ränder

mu = [0 0];
 Sigma = [1 0.3; 0.3 1];
%Sigma = [.25 .3; .3 1];
%Sigma = [.7 .3; .3 1];
% Sigma = [.7 .3; .3 1.5];
%Sigma = [.7 .6; .6 1.5]; % Kovarianz-Matrix, mit Varianzen auf Hauptdiagonale
%Sigma = [.7 .8; .8 1];
Sigma_mod=[sqrt(Sigma(1,1)) Sigma(1,2); Sigma(2,1) sqrt(Sigma(2,2))] % enthält Standardabweichungen auf Hauptdiagonale
rho=Sigma(2,1)/(Sigma(1,1)*Sigma(2,2)) %Pearson-Korrelationskoeffizient
x1 = -4:.2:4; x2 = -4:.2:4;
[X1,X2] = meshgrid(x1,x2);
F = mvnpdf([X1(:) X2(:)],mu,Sigma_mod); %Wahrscheinlichkeitsdichtefunktion einer multivariaten Normalverteilung ausgewertet auf Grid 
F = reshape(F,length(x2),length(x1));%in Matrixform bringen

figure(1)
surf(x1,x2,F);
caxis([min(F(:))-.5*range(F(:)),max(F(:))]);
axis([-3 3 -3 3 0 .4])
xlabel('x_1'); ylabel('x_2'); zlabel('Wktsdichte');
title('Dichte einer multivariaten Normalverteilung')

% Betrachtung der Ränder
figure(2)
subplot(2,1,1)%Erste Komponente
plot(x1,normpdf(x1,mu(1,1),Sigma_mod(1,1)));
xlabel('x_1'); ylabel('f(x_1)');
title('Rand X_1')
subplot(2,1,2)%Zweite Komponente
plot(x2,normpdf(x2,mu(1,2),Sigma_mod(2,2)));
xlabel('x_2'); ylabel('f(x_2)');
title('Rand X_2')

% Wie sieht Dichte aus für veränderte Parameter?

%% Gauss-Copula mit normalverteilten Rändern
% u1=normcdf(x1,mu(1,1),Sigma(1,1));
% u2=normcdf(x2,mu(1,2),Sigma(2,2));
u1=0:0.01:1;  % gleichverteilte ui aus U\simU(0,1)
u2=0:0.01:1;

[U1,U2] = meshgrid(u1,u2);
y = copulapdf('Gaussian',[U1(:),U2(:)],rho);%Copuladichte ausgewertet auf Grid mit Korrelationskoeffizienten rho

figure(3)
surf(U1,U2,reshape(y,length(U2),length(U1)))   
xlabel('u_1')
ylabel('u_2')
title('Gauss-Copula Dichte')

coppdf=reshape(y,length(U2),length(U1));
% ACHTUNG: Gauss-Copula Dichte hat andere Form als die Gauss-Dichte, hier sind jedoch auch
% Ränder nur auf (0,1) definiert und die Rücktransformation der Ränder steht noch aus.

% Rücktransformation zu multivariater Dichte
f_mv=zeros(length(U1));   % erzeugt quadratische matrix mit Nullen, für spätere Ergebnisse
X1_inv=zeros(length(U1)); % Ergebnisvektor für durch Invertieren rücktransformierte Xi
X2_inv=zeros(length(U1));
for i=1:length(U1);
    for j=1:length(U1);
f_mv(i,j)=normpdf(norminv(U1(i,j),mu(1,1),Sigma_mod(1,1)),mu(1,1),Sigma_mod(1,1))...  % norminv gibt die Inverse der cdf der NV
    *normpdf(norminv(U2(i,j),mu(1,2),Sigma_mod(2,2)),mu(1,2),Sigma_mod(2,2))...
    *coppdf(i,j);
X1_inv(i,j)=norminv(U1(i,j),mu(1,1),Sigma_mod(1,1));
X2_inv(i,j)=norminv(U2(i,j),mu(1,2),Sigma_mod(2,2));
    end
end

figure(4)
surf(X1_inv,X2_inv,f_mv)   
xlabel('x_1')
ylabel('x_2')
title('mv. Gauß-Dichte abgeleitet aus Gauss-Copula Dichte')


%% t-verteilte Ränder mit Abhängigkeitsstruktur einer Gauss-Copula

nu1=10;       % Anzahl der Freiheitsgrade der t-Verteilung für X1
nu2=20;       % Anzahl der Freiheitsgrade der t-Verteilung für X1
rho_t=0.34;   % Korrelation zwischen den beiden t-verteilten Zufallsvariablen (Parameter der Gauss-Copula)
alpha=2;     % Copula-Parameter für Clayton-Copula
u1=0:0.01:1;
u2=0:0.01:1;

[U1,U2] = meshgrid(u1,u2);
y = copulapdf('Gaussian',[U1(:),U2(:)],rho_t);
%y = copulapdf('Clayton',[U1(:),U2(:)],alpha);

figure(4)
surf(U1,U2,reshape(y,length(U2),length(U1)))   
xlabel('u_1')
ylabel('u_2')
title('Gauss-Copula Dichte')

coppdf=reshape(y,length(U2),length(U1));

f_mv=zeros(length(U1));   % erzeugt quadratische matrix mit Nullen, für spätere Ergebnisse
X1_inv=zeros(length(U1)); % Ergebnisvektor für durch Invertieren rücktransformierte Xi
X2_inv=zeros(length(U1));
for i=1:length(U1);
    for j=1:length(U1);
f_mv(i,j)=tpdf(tinv(U1(i,j),nu1),nu1)...  % tinv gibt die Inverse der cdf der tV
    *tpdf(tinv(U2(i,j),nu2),nu2)...
    *coppdf(i,j);
X1_inv(i,j)=tinv(U1(i,j),nu1);
X2_inv(i,j)=tinv(U2(i,j),nu2);
    end
end

figure(5)
surf(X1_inv,X2_inv,f_mv)   
xlabel('x_1')
ylabel('x_2')
zlabel('f(x_1,x_2)')
title('mv. Dichte mit t-Rändern abgeleitet aus Gauss-Copula Dichte')


