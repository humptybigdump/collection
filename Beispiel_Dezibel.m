%% Beispiel zur Fourier-Transformation mit Einheit Dezibel: Beispiel_Dezibel.m
% ============================================= 
% Signalverarbeitung in der Geodäsie - Uebung 3 
% ============================================= 
% SoSe2021, SoSe2022
% A. Heck

clear all;                      
close all;                      

%% Signalgenerierung

dt = 0.01;                      % Zeitschritte in s
T = 20;                         % Signallaenge in s
t = 0:dt:T-dt;                 

N = length(t);                  

r = rand(size(t));              % Rauschterm
                                
sig_t = 0.3 * cos(0.5*pi*t) + 0.4 * sin(3*pi*t) + r; % Signal in m
%sig_t = sig_t - mean(sig_t);   % --> Rueblick Uebungsblatt 2 - Aufgabe 2b

figure
plot(t,sig_t)                   
xlabel('Zeit t in s')      
ylabel('Amplitude sig(t) in m') 

%% Fourier-Transformation
                 
df=1/(N*dt);                    % Frequenzschritte in 1/s=Hz
fny=1/(2*dt);                   
       
Sig_f = dt * fftshift(fft(sig_t));  

f=-fny:df:fny-df;               

%% Amplitudenspektren

figure
plot(f,abs(Sig_f)/(N*dt))             
xlabel('Frequenz f in Hz')      
ylabel('Amplitude in ?') 
xlim([-20 20])
title('plot()')

figure
semilogy(f,abs(Sig_f)/(N*dt))             
xlabel('Frequenz f in Hz')      
ylabel('Amplitude in ?') 
xlim([-20 20])
title('semilogy()')

% figure
% plot(f,?)             
% xlabel('Frequenz f in Hz')      
% ylabel('Amplitude in ?') 
% xlim([-20 20])
% title('Dezibel')
