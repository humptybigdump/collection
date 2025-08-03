%% Beispiel zur Inverse-Fourier-Transformation:
% Beispiel_ifft.m
% =============================================
% Signalverarbeitung in der Geodaesie - Uebung 2
% =============================================
% SoSe22
% A. Ciesielski & A. Heck

clear all;                      % ?
close all;                      % ?

%% Signalgenerierung

dt = 0.01;                      % Zeitschritte in s
T = 20;                         % Signallaenge in s
t = 0:dt:T-dt;                  % ?

N = length(t);                  % ?

r = rand(size(t));              % Rauschterm
                                % rand() ?
sig_t = 0.3 * cos(0.5*pi*t) + 0.4 * sin(3*pi*t) + r;
% Signal in m
                                % Frequenz cos-Schwingung?
                                % Frequenz sin-Schwingung?

figure;                         % ?
plot(t,sig_t)                   % ?
xlabel('Zeitpunkt t in s')      % ?
ylabel('Amplitude sig(t) in m') % ?

%% Fourier-Transformation
                 
df=1/(N*dt);                    % Frequenzschritte in 1/s=Hz
fny=1/(2*dt);                   % ?

%Sig_f = dt * fft(sig_t);           % ?
Sig_f = dt * fftshift(fft(sig_t));  % ?

%f = 0:df:2*fny-df;             % Frequenzachse ohne fftshift
f=-fny:df:fny-df;               % ?

%(falls N ungerade --> f = -fny+df/2:df:fny-df/2;)



%% Amplituden- und Phasenspektrum

f=round(f,5);
df=round(df,5);

idx_0 = find(f==0);
idx_ny = find(f==fny-df);

Amp = [abs(Sig_f(idx_0)),2*abs(Sig_f(idx_0+1:idx_ny))] / T;
                                               % Warum *2?
                                               % Warum /T?
Pha = angle(round(Sig_f(idx_0:idx_ny),4));     % ?
                                               % in rad
                                
f2 = f(idx_0:idx_ny);           % Frequenzachse von 0 bis fny-df in Hz

figure
plot(f2,Amp)
xlabel('Frequenz f in Hz')
ylabel('Amplitude in m')
title('Amplitudenspektrum')
xlim([0 4])                         % ?

figure
plot(f2,Pha*180/pi)
xlabel('Frequenz f in Hz')
ylabel('Phase in deg')
title('Phasenspektrum')
axis([0 4 -180 180])                % ?

%% Inverse-Transform

dt2=1/(N*df);                    % Frequenzschritte in 1/s=Hz
fny=1/(2*dt2);                   % Nyquistfrequenz
T2 = N * dt2

Sig_t2 = ifft(ifftshift(Sig_f))/dt2;  % ?
% dt2 = dt

t2 = 0:dt2:T2-dt2;                  % ?

figure
plot(t2,Sig_t2)
xlabel('Zeitpunkt t in s')      % ?
ylabel('Amplitude sig(t) in m') % ?
title('Amplitude (zeitrehe reconstructed)')
xlim([0 T2])                         % ?


%% Differences

Sigdiff = abs(Sig_t2 - sig_t);
figure
semilogy(t,Sigdiff)
xlabel('Zeitpunkt t in s')      % ?
ylabel('Signal differenz in m') % ?
title('Amplitude differenz')
xlim([0 T2])                         % ?


%% Energy confirmation

%dt = 0.01; T=20;
scaleFactor=sqrt(T)/sqrt(dt);
% Using current variables
% sig_t energy:
Energy_t = norm(sig_t)
%Sig_f= dt * fftshift(fft(sig_t));
Energy_f2 = norm(Sig_f)/scaleFactor/dt
%Sig_t2 = ifft(ifftshift(Sig_f))/dt energy:
Energy_t2 = norm(Sig_t2)
% Amp = [abs(Sig_f(idx_0)),2*abs(Sig_f(idx_0+1:idx_ny))] / T;
% Sig_f mean phase:
mean_pha=mean(Pha)

%% Variables when energy is conserved:
Sig_f1 = fftshift(fft(sig_t))/scaleFactor;
Energy_f1 = norm(Sig_f1)
Sig_t1= ifft(ifftshift(Sig_f1))*scaleFactor;
Energy_t1 = norm(Sig_t1)

%% compare differences
Amp1 = [abs(Sig_f1(idx_0))/2,abs(Sig_f1(idx_0+1:idx_ny))] / T;
figure
plot(f2,(Amp1-Amp))
xlabel('Frequenz f in Hz')
ylabel('Amplitude differenz in m')            % Einheit ?
title('Amplitudenspektrumdifferenz')
xlim([0 4])                         % ?


Pha1 = angle(round(Sig_f1(idx_0:idx_ny),4));     % ?
figure
plot(f2,(Pha1-Pha))
xlabel('Frequenz f in Hz')
ylabel('Phasedifferenz in deg')            % Einheit ?
title('Phasedifferenz')
xlim([0 4])
