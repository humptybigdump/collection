
% x=[2 2 0 2 0 1 1 4 6 3]; % unsere Stichprobe
% B=1000 % Anzahl Bootstrapstichproben
% 
% A=x(randi(10,B,10)); % erzeugt B Bootstrap-Stichproben (pro Zeile eine Stichprobe)
% 
% T=mean(A,2);
% 
% [f,zi] = ksdensity(T);
% figure
% plot(zi,f);
% 
% T_1=sort(T);
% c=T_1(0.95*B) % einseitiger Test


x=[2 2 0 2 0 1 1 4 6 3]; % unsere Stichprobe

% simuliere aus Poisson mu=3
m=1000; % Anzahl gezogene Stichproben
V=poissrnd(3,m,10);
Z=mean(V,2)

[f,zi] = ksdensity(Z);
figure
plot(zi,f);

Z_1=sort(Z);
c_u=Z_1(0.025*m) 
c_o=Z_1(0.975*m)

T=mean(x)
