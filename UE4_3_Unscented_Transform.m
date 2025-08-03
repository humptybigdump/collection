%%%%%%%%%% Unscented Transform Aufgabe

yii=[3 3]';
Pii=0.8*diag([1 1])+0.2*ones(2,2); % ergibt Kovarainz-Matrix


p=length(yii);
n=2*p+1; %Anzahl der sigma-Punkte
kappa=3-p;




%f_schl = @(x) x.^2;
%f_schl = @(x) x.^2;
f_schl = @(x) sqrt(sum((x).^2,1));


%%%% u-Punkte bauen (zunächst für 1,-1 statt und in P=I Welt), die u's sind
%%%% spaltenvektoren
% zunächst der StützGenerator [0]
clear Gen; %falls es schon da ist, kann dieser Algo Probleme machen
Gen(1:p,1)=zeros(p,1);
% dann die Genartoren aus [u]
Gen=[Gen,eye(p),-eye(p)];

Gen=sqrt(kappa+p)*Gen;


%%%%%%% ok, jetzt müssen wir die gewichte bauen. bauen also einen Vektor w ,
%%%%%%% der zu den entsprechenden Spalten von Gen immer das entsprechende
%%%%%%% Gewicht liefert; für die gewichte vgl. Lerner
w(1)=kappa/(kappa+p);
w(2:2*p+1)=1/(2*(kappa+p));


% haben wir auch;

%%%%%%%%%% Initialisierung
% sigma-Punkte und Gewichte bauen für die Erstinitialisierung
% Danach werden die neuen Punkte immer nach dem Update gebaut.
% hier gehen jetzt zum ersten Mal problemspezifisch mu und P ein
%choles=real(sqrtm(Pii));%chol(Pii); %Pii ist schon die erweiterte
[V,D] = eig(Pii); % V: Matrix der Eigenvektoren, D: Matrix der Eigenwerte (Diagonalmatrix)
% es gilt Pii=V*D*V' -> Pii^1/2=V*D^(1/2)*V' ist die Matrixwurzel
choles=V*D^(1/2)*V'; %Choleski-Zerlegung

clear y
for j=1:n,
    y(:,j)=yii+choles*Gen(:,j);
end;

clear yi1i;

for j=1:n, yi1i(:,j)=f_schl(y(:,j));     end;

%jetzt Mittelwert und Kovarianz davon berechnen

yschaetz=zeros(length(yi1i(:,1)),1);
for j=1:n, yschaetz(:)=yschaetz(:)+w(j)*yi1i(:,j); end; %reinen Schätzwert ausgeben (nicht erweitert)
yschaetz

Pschaetz=zeros(length(yi1i(:,1)),length(yi1i(:,1)));
for j=1:n, Pschaetz(:,:)=Pschaetz(:,:)+w(j)*(yi1i(:,j)-yschaetz(:))*(yi1i(:,j)-yschaetz(:))'; end; %reinen Schätzwert ausgeben (nicht erweitert)
Pschaetz

% Mache das simulativ
N=100000;
Sample=yii*ones(1,N)+choles*randn(p,N);

Sample_schl=f_schl(Sample);
mean(Sample_schl')
cov(Sample_schl')


% genaue Generatoren
%%%% u-Punkte bauen (zunächst für 1,-1 statt und in P=I Welt), die u's sind
%%%% spaltenvektoren
% zunächst der StützGenerator [0]
clear Gen; %falls es schon da ist, kann dieser Algo Probleme machen
Gen(1:p,1)=zeros(p,1);
% dann die Genartoren aus [u]
Gen=[Gen,eye(p),-eye(p)];

% ok, haben wir. Jetzt die [u,u]
Gen=[Gen,unique(perms([-1,-1,zeros(1,p-2)]),'rows')',unique(perms([1,1,zeros(1,p-2)]),'rows')',unique(perms([-1,1,zeros(1,p-2)]),'rows')'];

Gen=sqrt(kappa+p)*Gen;
%%%%%%%%%%% haben wir!

n=2*p^2+1;
%%%%%%% Gewicht liefert; für die gewichte vgl. Lerner
w(1)=1+(p^2-7*p)/18;
w(2:2*p+1)=(4-p)/18;
w(2*p+2:n)=1/36;
%%%%%%%%%% Initialisierung
% sigma-Punkte und Gewichte bauen für die Erstinitialisierung
% Danach werden die neuen Punkte immer nach dem Update gebaut.
% hier gehen jetzt zum ersten Mal problemspezifisch mu und P ein
choles=real(sqrtm(Pii));%chol(Pii); %Pii ist schon die erweiterte
clear y
for j=1:n,
    y(:,j)=yii+choles*Gen(:,j);
end;

clear yi1i;

for j=1:n, yi1i(:,j)=f_schl(y(:,j));     end;

%jetzt Mittelwert und Kovarianz davon berechnen

yschaetz=zeros(length(yi1i(:,1)),1);
for j=1:n, yschaetz(:)=yschaetz(:)+w(j)*yi1i(:,j); end; %reinen Schätzwert ausgeben (nicht erweitert)
yschaetz

Pschaetz=zeros(length(yi1i(:,1)),length(yi1i(:,1)));
for j=1:n, Pschaetz(:,:)=Pschaetz(:,:)+w(j)*(yi1i(:,j)-yschaetz(:))*(yi1i(:,j)-yschaetz(:))'; end; %reinen Schätzwert ausgeben (nicht erweitert)
Pschaetz
