close all;
%System
A=[0 1 0;-1 -1 1;1 1 -2];
B = [0 0; 1 0; 0 1];
C = [1 1 0; 0 0 1];
x0 = [0;0;0];
sys=ss(A,B,C,zeros(2,2));

%Simulationsparameter
t=[0:0.01:20]'; 

%Stellsignal
%nach 2 Sekunden lauten die gew�nschten Ausgangswerte [2;0] und nach 5
%Sekunden [2;-1]
%Die zugeh�rigen Stellwerte k�nnen �ber inv(dcgain(sys))*([2;0] bzw. [2;-1]) berechnet
%werden und lauten [2;-2] bzw. [3;-4]
yd = [zeros(1,200),2*ones(1,1801);  zeros(1,500) -ones(1,1501)];

ud = [[zeros(1,200),2*ones(1,300), 3*ones(1,1501)];  [zeros(1,200), -2*ones(1,300), -4*ones(1,1501)]];
[y,t,x]=lsim(sys,ud,t,x0);

%plot
figure(1);
plot(t,yd(1,:),t,y(:,1),t,yd(2,:),t,y(:,2),'linewidth',2);hold on;
plot(t,ud,'linewidth',2);
axis([0 20 -4 3]);
title('Strecke ohne Regler');
legend('y_{d,1}','y_1','y_{d,2}','y_2','u_1','u_2');
xlabel('Zeit in s');
ylabel('Stellverlauf und Istverlauf');


%Deutlich ist die Verkopplung zu erkennen und das langsame Einschwingen der
%Strecke --> ein Regler muss her
%Wir beginnen mit dem LQ-Regler, den wir kennen

%Entwurf LQ-Regler
t=[0:0.01:10]'; %jetzt k�rzer, da Regler das Einschwingen beschleunigt
%Sollsignal
yd = [zeros(1,200),2*ones(1,801);  zeros(1,500) -ones(1,501)];


Q=eye(3);
R=eye(2);
K=lqr(A,B,Q,R);
F=inv(C*inv(-A+B*K)*B);

sys_LQ=ss(A-B*K,B*F,C,zeros(2,2));
[y,t,x]=lsim(sys_LQ,yd,t,x0);

%plot
figure(2);
plot(t,yd(1,:),t,y(:,1),t,yd(2,:),t,y(:,2),'linewidth',2);
axis([0 10 -2 3]);
title('Regelkreis mit LQ-Regler');
legend('y_{d,1}','y_1','y_{d,2}','y_2');
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf');
%Deutlich ist zu erkennen, dass das Einschwingen der geregelten Strecke
%viel schneller geht. Kopplungen sind aber noch zu erkennen
%Weil uns das st�rt, entwerfen wir nun einen Entkopplungsregler

%Entkopplungsregler
E = C*B;
H = [ C(1,:) * A + 2 * C(1,:); C(2,:) * A + 3 * C(2,:)];
K = inv(E)*H;
F = inv(E) * [2 0 ; 0 3];

sys_entkoppelt=ss(A-B*K,B*F,C,zeros(2,2)); 
[y,t,x]=lsim(sys_entkoppelt,yd,t,x0);

figure(3);
plot(t,yd(1,:),t,y(:,1),t,yd(2,:),t,y(:,2),'linewidth',2);
axis([0 10 -2 3]);
title('Regelkreis mit Entkopplungsregler');
legend('y_{d,1}','y_1','y_{d,2}','y_2');
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf');

% check Stabilit�t
EW_Bsp1_Entkopplung = eig(A-B*K)
%--> passt


