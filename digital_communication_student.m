%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DIGITAL COMMUNICATION
% 
% Modern Radio System Engineering (MRSE)
% Tutorial 1
%
% Author: Joerg Eisenbeis, IHE, Karlsruhe Institute of Technology
% Mail:   joerg.eisenbeis@kit.edu
% Date:   01/Dec/2015

% Update: Xueyun Long, IHE, Karlsruhe Institute of Technology
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close all;           % Close all open matlab figures
clear all;           % Clear all variables from workspace
clc                  % Clear console output
%% Parameter Initialization
% Constants
Ns  = 1e6;           % Number of processed symbols
%
Fs  = 1e9;           % Symbol rate
Mod = 'QAM';         % Modulation scheme: 'QAM','PSK'
M   = 16;            % Size of signal constellation
%
Filter  = 'RRC';     % Filter type: 'RRC', 'RECT'
Rolloff = 0.25;      % Roll-Off-Factor for RRC-Filter
L   = 40;            % Filter length
Osr = 4;             % Oversampling factor
%
Snr  = 40;
% EbN0 = 40;           % Channel Eb/N0
% Variables
fa  = Fs*Osr;        % Sample rate

%% Check Input Parameters
if ~(strcmp(Mod,'PSK') || strcmp(Mod,'QAM'))
    error('ERROR 01: Modulation scheme is not supported');
end
if (log2(M) ~= round(log2(M)))
    error('ERROR 02: Wrong modulation size');
end
if ~(strcmp(Filter,'RRC') || strcmp(Filter,'RECT'))
    error('ERROR 03: Filter type is not supported');
end

%-------------------------------------------------------------------------%
%% Signal Source
% Create random binary data stream as a column vector.
tx_bin   = randi([0 1],Ns.*log2(M),1); % Random binary data stream
%% Bit-to-Symbol Mapping
% Convert the bits in x into k-bit symbols.
tx_sym = bit2int(tx_bin,log2(M));
%% Modulation
if strcmp(Mod,'PSK') % PSK modulation
    tx_mod = pskmod(tx_sym, M);
    tx_mod = tx_mod./max(abs(tx_mod));
elseif strcmp(Mod,'QAM') % QAM modulation
    tx_mod = qammod(tx_sym, M);
    tx_mod = tx_mod./ max(abs(tx_mod));
end

%% TX Filter
if strcmp(Filter,'RRC')
    % RRC-Filter Design
    shape   = 'Square Root Raised Cosine';
    f = fdesign.pulseshaping(Osr, shape,'Nsym,beta', L, Rolloff);
    H = design(f,'window');                       % Design Filter
    H.Numerator = H.Numerator / max(H.Numerator); % Normalize Filter
    % filter the upsampled signal
    tx_sig = filter(H, upsample([tx_mod; zeros(L/2,1)], Osr));

elseif strcmp(Filter,'RECT')
    tx_sig = rectpulse(tx_mod,Osr);
else
    error('Error: Filter does not exist!');
end

%% AWGN channel with fixed Eb/N0
% snr = EbN0 - 10*log10(Osr) + 10*log10(log2(M));
hChan = comm.AWGNChannel('NoiseMethod', 'Signal to noise ratio (SNR)', 'SNR',Snr-10*log10(Osr));
hChan.SignalPower = (tx_sig' * tx_sig)/ length(tx_sig); % [W] Input signal power
x_ch = step(hChan,tx_sig);

%% RX Filtering
if strcmp(Filter,'RRC') % RRC filter and downsampling
    rx_filt = filter(H,x_ch);           % Use same filter as tx-filter
    rx_filt = downsample(rx_filt ,Osr); % Downsample signal
    rx_filt = rx_filt(L+1:end);         % Account for delay
elseif strcmp(Filter,'RECT') % Downsampling
    rx_filt = downsample(x_ch,Osr);
end
% Normalize for demodulation ("Automatic Gain Control")
RxSignalPower = (rx_filt' * rx_filt)/ length(rx_filt); % [W] Input signal power
rx_filt = rx_filt ./ sqrt(RxSignalPower);              % Normalize power to 1

%% Demodulation
RxNorm = 1;
if strcmp(Mod,'PSK') % PSK demodulation
    rx_sym = pskdemod(rx_filt,M);
elseif strcmp(Mod,'QAM') % QAM demodulation
    % Calculate RxNorm
     qamdeConstellation = qammod(0:M-1, M, 'gray');
     RxNorm = sqrt(qamdeConstellation * qamdeConstellation' ...
         /length(qamdeConstellation));
     rx_filt = rx_filt .* RxNorm;
    rx_sym = qamdemod(rx_filt,M,'gray');
end
%% Symbol-to-Bit Mapping
% Undo the bit-to-symbol mapping performed earlier.
rx_bin = int2bit(rx_sym, log2(M));

%-------------------------------------------------------------------------%
%% Account for filter delay
if strcmp(Filter,'RRC')
    Delay     = L/2;
    ref_sym = tx_mod(1:end-Delay);
    Delay_bin = L/2*log2(M);
    ref_bin = tx_bin(1:end-Delay_bin);
elseif strcmp(Filter,'RECT') 
    ref_sym = tx_mod;
    ref_bin = tx_bin;
end
%% Normalize reference symbols
REF_SignalPower = (ref_sym' * ref_sym)/ length(ref_sym);
ref_sym = ref_sym ./ sqrt(REF_SignalPower) .* RxNorm;
%% Signal quality analysis
% BER calculation
% Compare binary input with binary output data stream to obtain the 
% number of errors and the bit error rate.
hErrorCalc = comm.ErrorRate;
BER = step(hErrorCalc,ref_bin,rx_bin);
disp(['BER       = ',num2str(BER(1))]);
% SNR Estimation
hMER = comm.MER('MinimumMEROutputPort', true);
SNR = step(hMER,ref_sym,rx_filt);
disp(['SNR       = ',num2str(SNR),' dB']);

