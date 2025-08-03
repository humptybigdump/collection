function [h, H] = ga (t,f,T)
% In optical communication systems, different linear pulse-shaping filters 
% are used to generate continuous-time transmit signals. We can define 
% these filters either through their impulse response hp(t) or their 
% frequency response Hp( f )=F{hp(t)}, where F{ } stands for the Fourier
% transform.

% ga: Gaussian Filter
% Time Domain
h = exp(-4*log(2)*t.^2/T.^2);

% Frequency Domain
H = sqrt(pi/(4*log(2)/T.^2))*exp(-pi^2*f.^2./(4*log(2)/T.^2));
end